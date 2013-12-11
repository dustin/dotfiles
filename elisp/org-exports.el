(load
 (expand-file-name "~/elisp/org-export-generic.el"))

; Export for google code
(org-set-generic-type
 "googlecode"
 '(:file-suffix        	    ".txt"
     :key-binding                   ?G

     :header-prefix            	    ""
     :header-suffix            	    ""

     :title-format             	    "= %s =\n"

     :date-export        	        t
     :date-format                   "Date: %s\n"

     :toc-export                    nil

     :body-header-section-numbers   nil
     :body-section-prefix           "\n"

     :body-section-header-prefix    ("== " "=== "
				     "==== " "===== " "====== ")
     :body-section-header-suffix    (" ==\n" " ===\n"
				     " ====\n" " =====\n" " ======\n")

     :body-line-export-preformated  t          ;; yes/no/maybe???
     :body-line-format              "%s\n"
     :body-line-wrap                75

     :body-line-fixed-prefix       "{{{\n"
     :body-line-fixed-format       "%s\n"
     :body-line-fixed-suffix       "}}}\n"

     :body-list-format              "  * %s\n"
     :body-number-list-format       "  # %s\n"

     :body-bullet-list-prefix       ("* " "** " "*** " "**** " "***** ")
   ))

; Export for twiki
(org-set-generic-type
 "twiki"
 '(:file-suffix        	    ".txt"
     :key-binding                   ?T

     :header-prefix            	    ""
     :header-suffix            	    ""

     :title-format             	    ""

     :date-export        	        t
     :date-format                   ""

     :toc-export                    nil

     :body-header-section-numbers   nil
     :body-section-prefix           "\n"

     :body-section-header-prefix    ("---+ " "---++ "
				     "---+++ " "---++++ " "---+++++ ")
     :body-section-header-suffix    ("\n" "\n" "\n" "\n" "\n")

     :body-line-export-preformated  t          ;; yes/no/maybe???
     :body-line-format              "%s\n"
     :body-line-wrap                75

     :body-line-fixed-prefix       "<verbatim>\n"
     :body-line-fixed-format       "%s\n"
     :body-line-fixed-suffix       "</verbatim>\n"

     :body-list-format              "%s\n"
     :body-number-list-format       "  # %s\n"

     :body-bullet-list-prefix       ("   * " "      * " "         * "
                                     "            * " "               * ")
   ))

; Export for
(org-set-generic-type
 "confluence"
 '(:file-suffix        	    ".txt"
     :key-binding                   ?C

     :header-prefix            	    ""
     :header-suffix            	    ""

     :title-format             	    "{panel:bgColor=#f0f0f0}\n{toc:outline=true|style=none|indent=10px}\n{panel}\n\n"

     :date-export        	        t
     :date-format                   ""

     :toc-export                    nil

     :body-header-section-numbers   nil
     :body-section-prefix           ""

     :body-section-header-prefix    ("h1. " "h2. " "h3. " "h4. " "h5. ")
     :body-section-header-suffix    ("\n" "\n" "\n" "\n" "\n")

     :body-line-export-preformated  t          ;; yes/no/maybe???
     :body-line-format              "%s\n"
     :body-line-wrap                75

     :body-line-fixed-prefix       "{{"
     :body-line-fixed-format       "%s"
     :body-line-fixed-suffix       "}}"

     :body-list-format              "%s\n"
     :body-number-list-format       "# %s\n"

     :body-bullet-list-prefix       ("* " "** " "*** " "**** " "***** ")
   ))
