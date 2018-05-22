(require 'ox-taskjuggler)

(defun org-taskjuggler--build-task (task info)
  "Return a task declaration.

TASK is a headline.  INFO is a plist used as a communication
channel.

All valid attributes from TASK are inserted.  If TASK defines
a property \"task_id\" it will be used as the id for this task.
Otherwise it will use the ID property.  If neither is defined
a unique id will be associated to it."
  (let* (

	 ;;(allocate (org-element-property :ALLOCATE task))

	 ;; MTA hack: grap MYGROUPS property from project as associative list and use that for lookup
	 ;; if allocation begins with "###"
	 (allocate (let ((groups
			  (eval (car (read-from-string
				      (org-element-property :MYGROUPS (org-taskjuggler-get-project info))))))
			 (taskalloc (org-element-property :ALLOCATE task)))
		     (if taskalloc
			 (if (string-prefix-p "{{{" taskalloc)
			     (let ((group (substring taskalloc 3 nil)))
			       (cdr (assoc group groups)))
			   taskalloc))))

         (complete
          (if (eq (org-element-property :todo-type task) 'done) "100"
            (org-element-property :COMPLETE task)))
         (depends (org-taskjuggler-resolve-dependencies task info))
         (effort (let ((property
			(intern (concat ":" (upcase org-effort-property)))))
		   (org-element-property property task)))
         (milestone
          (or (org-element-property :MILESTONE task)
              (not (or (org-element-map (org-element-contents task) 'headline
			 'identity info t)  ; Has task any child?
		       effort
		       (org-element-property :LENGTH task)
		       (org-element-property :DURATION task)
		       (and (org-taskjuggler-get-start task)
			    (org-taskjuggler-get-end task))
		       (org-element-property :PERIOD task)))))
         (priority
          (let ((pri (org-element-property :priority task)))
            (and pri
                 (max 1 (/ (* 1000 (- org-lowest-priority pri))
                           (- org-lowest-priority org-highest-priority)))))))

    (concat
     ;; Opening task.
     (format "task %s \"%s\" {\n"
             (org-taskjuggler-get-id task info)
             (org-taskjuggler-get-name task))
     ;; Add default attributes.
     (and depends
          (format "  depends %s\n"
                  (org-taskjuggler-format-dependencies depends task info)))
     (and allocate
          (format "  purge %s\n  allocate %s\n"
                  ;; Compatibility for previous TaskJuggler versions.
                  (if (>= org-taskjuggler-target-version 3.0) "allocate"
                    "allocations")
                  allocate))
     (and complete (format "  complete %s\n" complete))
     (and effort
          (format "  effort %s\n"
                  (let* ((minutes (org-duration-to-minutes effort))
                         (hours (/ minutes 60.0)))
                    (format "%.1fh" hours))))
     (and priority (format "  priority %s\n" priority))
     (and milestone "  milestone\n")
     ;; Add other valid attributes.
     (org-taskjuggler--indent-string
      (org-taskjuggler--build-attributes
       task org-taskjuggler-valid-task-attributes))
     ;; Add inner tasks.
     (org-taskjuggler--indent-string
      (mapconcat 'identity
                 (org-element-map (org-element-contents task) 'headline
                   (lambda (hl) (org-taskjuggler--build-task hl info))
                   info nil 'headline)
                 ""))
     ;; Closing task.
     "}\n")))

;; set project properties to taskjuggler defaults
(setq org-taskjuggler-default-global-properties "
leaves holiday \"Public holiday\" 2018-05-10 +2d,
       holiday \"Public holiday\" 2018-05-21 +1d,
       holiday \"Public holiday\" 2018-06-05 +1d
")

(setq org-taskjuggler-default-reports
      (quote
       ("
include \"tasks-reports.tji\"
include \"tasks-bookings.tji\"")))

;; add "include"
(setq org-taskjuggler-valid-project-attributes
      '(timingresolution timezone alertlevels currency currencyformat
			 dailyworkinghours extend includejournalentry now numberformat
			 outputdir scenario shorttimeformat timeformat trackingscenario
			 weekstartsmonday weekstartssunday workinghours
			 yearlyworkingdays include))
