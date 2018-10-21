(rule
 (targets problem_008.ml)
 (deps "008.txt")
 (action (with-stdout-to %{targets} (run ./gen-problem.sh 008))))

(rule
 (targets problem_011.ml)
 (deps "011.txt")
 (action (with-stdout-to %{targets} (run ./gen-problem.sh 011))))

(rule
 (targets problem_013.ml)
 (deps "013.txt")
 (action (with-stdout-to %{targets} (run ./gen-problem.sh 013))))

(rule
 (targets problem_018.ml)
 (deps "018.txt")
 (action (with-stdout-to %{targets} (run ./gen-problem.sh 018))))

(rule
 (targets problem_022.ml)
 (deps "022.txt")
 (action (with-stdout-to %{targets} (run ./gen-problem.sh 022))))

(rule
 (targets problem_042.ml)
 (deps "042.txt")
 (action (with-stdout-to %{targets} (run ./gen-problem.sh 042))))

(rule
 (targets problem_067.ml)
 (deps "067.txt")
 (action (with-stdout-to %{targets} (run ./gen-problem.sh 067))))

(rule
 (targets problem_105.ml)
 (deps "105.txt")
 (action (with-stdout-to %{targets} (run ./gen-problem.sh 105))))

