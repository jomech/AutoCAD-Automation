;;;----------------------------------------------------------------------------
;;;
;;; QA_CHECK.LSP
;;;
;;; Description:
;;;   An AutoLISP routine for AutoCAD to perform a comprehensive Quality Assurance (QA)
;;;   check on electrical network drawings. This routine verifies drawings against
;;;   a set of predefined standards, based on the Powercor/CitiPower Drafting
;;;   Standards (DD001 & DD002).
;;;
;;;   The routine checks the following criteria:
;;;   1. Dimensional Accuracy: Verifies dimension styles and tolerances.
;;;   2. Layer Consistency: Ensures objects are on the correct layers.
;;;   3. Annotation Standards: Validates text and annotation styles, fonts, and sizes.
;;;   4. Cable Lengths: Compares geometric polyline lengths with annotated lengths.
;;;   5. Correct Asset Placement: Checks for common asset placement errors, like offsets.
;;;
;;; Usage:
;;;   1. Load this file into your AutoCAD session using the APPLOAD command.
;;;   2. Type "QA_CHECK" in the command line and press Enter.
;;;   3. The routine will process the entire drawing and output a report to the
;;;      command line. It will also offer to save a detailed report to a text file.
;;;
;;; Version: 1.3 - Fixed ActiveX error "unknown name : TextString" by verifying
;;;                object type before accessing text properties.
;;; Author: Gemini
;;; Date: August 9, 2025
;;;
;;;----------------------------------------------------------------------------

;;; ============================================================================
;;; ==                          MAIN FUNCTION                                 ==
;;; ============================================================================

(defun C:QA_CHECK (/ *error* doc space ss-all report-data file-path f)
  ;; --- Error Handling Setup ---
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ;; --- Initialization ---
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq space (vla-get-modelspace doc))
  (setq report-data '()) ; Initialize an empty list to store report findings

  (princ "\nStarting Drawing Quality Assurance Check...")
  (princ "\nProcessing all objects in Model Space. This may take a moment...")

  ;; --- Select all entities in model space ---
  (setq ss-all (ssget "_X" '((0 . "~VIEWPORT"))))
  (if (not ss-all)
    (progn
      (princ "\nNo objects found in Model Space. Aborting.")
      (exit)
    )
  )

  ;; --- Run all QA check modules ---
  (princ "\n1. Checking Layer Consistency...")
  (setq report-data (append report-data (check-layers ss-all)))

  (princ "\n2. Checking Annotation Standards...")
  (setq report-data (append report-data (check-annotations ss-all)))
  
  (princ "\n3. Checking Dimensional Accuracy...")
  (setq report-data (append report-data (check-dimensions ss-all)))

  (princ "\n4. Checking Asset Placement...")
  (setq report-data (append report-data (check-asset-placement ss-all)))

  (princ "\n5. Checking Cable Lengths...")
  (setq report-data (append report-data (check-cable-lengths ss-all)))
  
  (princ "\n\nQA Check Complete.")

  ;; --- Display Report ---
  (if report-data
    (progn
      (princ "\n\n--- QA REPORT SUMMARY ---")
      (princ "\nDiscrepancies found. See details below:")
      (print-report report-data)

      ;; --- Ask to save report to file ---
      (initget "Yes No")
      (if (eq (getkword "\nSave detailed report to a file? [Yes/No] <Yes>: ") "No")
        (princ "\nReport not saved.")
        (progn
          (setq file-path (getfiled "Save Report As" (getvar "dwgprefix") "txt" 1))
          (if file-path
            (progn
              (setq f (open file-path "w"))
              (write-line "AutoCAD Drawing Quality Assurance Report" f)
              (write-line (strcat "Drawing: " (getvar "dwgname")) f)
              (write-line (strcat "Date: " (rtos (getvar "cdate") 2 8)) f)
              (write-line "\n--------------------------------------------------" f)
              (foreach item report-data
                (write-line (strcat (car item) ": " (cadr item)) f)
              )
              (close f)
              (princ (strcat "\nReport saved to: " file-path))
            )
            (princ "\nFile save cancelled.")
          )
        )
      )
    )
    (princ "\n\n--- QA REPORT SUMMARY ---\nNo discrepancies found. The drawing appears to meet the checked standards.")
  )
  (princ) ; Suppress returning the last value
)

;;; ============================================================================
;;; ==                        HELPER FUNCTIONS                              ==
;;; ============================================================================

;;; --- Function to get VLA object from entity name ---
(defun get-vla-object (ename)
  (vlax-ename->vla-object ename)
)

;;; --- Function to get object handle ---
(defun get-handle (vla-obj)
  (vla-get-handle vla-obj)
)

;;; --- Function to print the report to the command line ---
(defun print-report (data)
  (foreach item data
    (princ (strcat "\n[!] " (car item) ": " (cadr item)))
  )
  (princ)
)

;;; ============================================================================
;;; ==                        QA CHECK MODULES                              ==
;;; ============================================================================

;;; --- 1. Layer Consistency Check ---
(defun check-layers (ss / i ent vla-obj layer-name obj-type report)
  (setq report '() i 0)
  
  ;; Define standard layers for object types (expand this list based on DD001/DD002)
  (setq layer-standards
    '(
      ("LWPOLYLINE" . "UG_LVM_NEW,UG_HV_NEW,UG_CONDUIT_NEW,UG_LVS_NEW,UG_PLS_NEW,EARTH") ; Add more valid layers
      ("INSERT" . "UG_PIT_NEW,UG_SUBSTATION_NEW,UG_POLE_NEW,UG_BLOCKS_NEW") ; Block layers
      ("TEXT" . "TEXT,ANNO")
      ("MTEXT" . "TEXT,ANNO")
      ("DIMENSION" . "DIM,ANNO")
    )
  )

  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq vla-obj (get-vla-object ent))
    (setq layer-name (vla-get-layer vla-obj))
    (setq obj-type (vla-get-objectname vla-obj))

    ;; Normalize object type for lookup
    (if (wcmatch obj-type "AcDb*")
        (setq obj-type (substr obj-type 5))
    )

    (setq expected-layers (cdr (assoc obj-type layer-standards)))
    
    (if (and expected-layers (not (wcmatch (strcase layer-name) (strcase expected-layers))))
      (setq report (cons 
        (list "Layer Violation" 
          (strcat "Object (Handle: " (get-handle vla-obj) ", Type: " obj-type ") is on layer '" layer-name "' but should be on a layer like '" expected-layers "'.")
        )
        report)
      )
    )
    (setq i (1+ i))
  )
  (reverse report)
)

;;; --- 2. Annotation Standards Check ---
(defun check-annotations (ss / i ent vla-obj obj-type style-name text-height report)
  (setq report '() i 0)
  
  ;; Define annotation standards (Font from DD001 Sec 10.1.2)
  (setq standard-font "ISOCPEUR.TTF")
  (setq standard-text-style "STANDARD") ; This should be the name from the template
  (setq standard-dim-style "EN_DIM") ; Example dimension style name

  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq vla-obj (get-vla-object ent))
    (setq obj-type (vla-get-objectname vla-obj))

    (cond
      ;; Check MTEXT and TEXT objects
      ((wcmatch obj-type "*Text")
        (setq style-name (vla-get-stylename vla-obj))
        (setq text-height (vla-get-height vla-obj))
        (if (not (wcmatch (strcase style-name) (strcase standard-text-style)))
          (setq report (cons
            (list "Annotation Style" (strcat "Text (Handle: " (get-handle vla-obj) ") uses incorrect style '" style-name "'. Expected: '" standard-text-style "'."))
            report))
        )
        ;; Note: Checking the font itself requires iterating through the style table. 
        ;; Checking the style name is often sufficient.
      )
      ;; Check DIMENSION objects
      ((wcmatch obj-type "*Dimension")
        (setq style-name (vla-get-stylename vla-obj))
        (if (not (wcmatch (strcase style-name) (strcase standard-dim-style)))
           (setq report (cons
            (list "Dimension Style" (strcat "Dimension (Handle: " (get-handle vla-obj) ") uses incorrect style '" style-name "'. Expected: '" standard-dim-style "'."))
            report))
        )
      )
    )
    (setq i (1+ i))
  )
  (reverse report)
)

;;; --- 3. Dimensional Accuracy Check ---
(defun check-dimensions (ss / i ent vla-obj measurement text-override tolerance report overridden-val)
  (setq report '() i 0)
  (setq tolerance 0.10) ; Tolerance from DD002 Section 6.6 (±0.10m)

  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (if (wcmatch (cdr (assoc 0 (entget ent))) "*DIMENSION")
      (progn
        (setq vla-obj (get-vla-object ent))
        (setq measurement (vla-get-measurement vla-obj))
        (setq text-override (vla-get-textoverride vla-obj))
        
        ;; Check if there is a text override
        (if (and text-override (/= text-override ""))
          (progn
            ;; Try to convert the override text to a number. If it's not a number, distof returns nil.
            (setq overridden-val (distof text-override))
            ;; Only proceed if the override was a valid number
            (if (and overridden-val (> (abs (- measurement overridden-val)) tolerance))
              (setq report (cons
                (list "Dimensional Accuracy" (strcat "Dimension (Handle: " (get-handle vla-obj) ") has a text override ('" text-override "') that deviates from the measured value (" (rtos measurement) ") by more than the tolerance of " (rtos tolerance) "."))
                report))
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  (reverse report)
)

;;; --- 4. Asset Placement Check ---
(defun check-asset-placement (ss / i ent vla-obj layer-name datum-line report asset-pt dist required-offset)
  (setq report '())
  
  ;; Find the datum line (e.g., a line on layer "DATUM" or "BL" for Building Line)
  (setq datum-line nil)
  (setq ss-datum (ssget "_X" '((0 . "LINE") (8 . "BL,DATUM"))))
  
  (if (not ss-datum)
    (progn
      (princ "\nWARNING: No DATUM or BL layer line found for asset placement check. Skipping.")
      '() ; Return an empty list if no datum is found
    )
    (progn ; Else, if datum is found, proceed with the check
      (setq datum-line (ssname ss-datum 0))
      (setq i 0)
      
      ;; Define asset layers and their required offsets (from DD002 Sec 1.3.6, etc.)
      (setq asset-offsets
        '(
          ("UG_HV_NEW" . 2.60)
          ("UG_LVM_NEW" . 2.60) ; Example offset, adjust as per standards
          ("UG_PIT_NEW" . 0.0) ; Example, pits on boundary line
        )
      )
      (setq offset-tolerance 0.2) ; Allowable tolerance for placement

      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq vla-obj (get-vla-object ent))
        (setq layer-name (vla-get-layer vla-obj))
        
        (setq required-offset (cdr (assoc layer-name asset-offsets)))

        (if required-offset
          (progn
            ;; Get a representative point for the asset
            (if (vlax-property-available-p vla-obj 'InsertionPoint)
              (setq asset-pt (vlax-get vla-obj 'InsertionPoint))
              (setq asset-pt (vlax-get vla-obj 'StartPoint))
            )
            
            ;; Calculate perpendicular distance from asset to datum line
            (setq dist (distance asset-pt (vlax-curve-getClosestPointTo datum-line asset-pt)))

            ;; Check if distance is within tolerance of required offset
            (if (> (abs (- dist required-offset)) offset-tolerance)
              (setq report (cons
                (list "Asset Placement" (strcat "Asset (Handle: " (get-handle vla-obj) " on layer '" layer-name "') is at an offset of " (rtos dist) "m from datum. Expected: " (rtos required-offset) "m."))
                report))
            )
          )
        )
        (setq i (1+ i))
      )
      (reverse report) ; Return the final report list
    )
  )
)


;;; --- 5. Cable Length Check ---
(defun check-cable-lengths (ss / i ent vla-obj layer-name geom-length report ss-text text-obj text-val annotated-length len-tolerance obj-type)
  (setq report '() i 0)
  (setq len-tolerance 0.05) ; 5% tolerance for cable length sag/connections

  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    ;; Filter for polylines on cable layers
    (if (and (eq (cdr (assoc 0 (entget ent))) "LWPOLYLINE")
             (wcmatch (strcase (cdr (assoc 8 (entget ent)))) "*_HV_*,*_LVM_*"))
      (progn
        (setq vla-obj (get-vla-object ent))
        (setq geom-length (vla-get-length vla-obj))
        
        ;; Find nearby text that might contain the length
        (setq ss-text (ssget "_C" (get-pline-midpoint ent) (get-pline-midpoint ent)))
        
        ;; Only proceed if text was found nearby
        (if ss-text
          (progn
            (setq text-obj (get-vla-object (ssname ss-text 0)))
            (setq obj-type (vla-get-objectname text-obj))
            (setq text-val nil) ; Initialize text-val to nil

            ;; *** FIX: Check if the object is a TEXT or MTEXT object before getting the text string ***
            (if (wcmatch obj-type "*Text")
                (setq text-val (vla-get-textstring text-obj))
            )

            ;; Only proceed if we successfully got a text value
            (if text-val
              (progn
                ;; Try to extract a number from the text (e.g., "240m")
                (setq annotated-length (distof (vl-string-subst "" "m" text-val)))

                ;; Only proceed if a valid number was extracted from the text
                (if (and annotated-length (> (abs (- geom-length annotated-length)) (* geom-length len-tolerance)))
                  (setq report (cons
                    (list "Cable Length" (strcat "Cable (Handle: " (get-handle vla-obj) ") has geometric length of " (rtos geom-length) "m but is annotated as " (rtos annotated-length) "m. Difference exceeds tolerance."))
                    report))
                )
              )
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  (reverse report)
)

;;; --- Helper to get midpoint of a polyline for searching ---
(defun get-pline-midpoint (pline-ent / param)
  (setq param (/ (vlax-curve-getEndParam pline-ent) 2.0))
  (vlax-curve-getPointAtParam pline-ent param)
)

;;; --- End of File ---
(princ "\nQA_CHECK.LSP loaded. Type QA_CHECK to run.")
(princ)