; First section is completed functions from lab 2
; =========================================================
; Checks if given list is equivalent to the goal state
; @param    state    List containing state of puzzle
; @return   T if the list is the goal state, nil otherwise
(defun goal-state (state)
    (if (equal state '(1 2 3 8 e 4 7 6 5))
        T
        nil
    )
)

; Returns the direction of a given move
; @param    move     List containing direction & resulting state of a move
; @returns  The direction of the move
(defun get-direction (move)
    (car move)
)

; Returns the state after a given move
; @param    move    List containing direction & resulting state of a move
; @return   The resulting state of the move
(defun get-state (move)
    (car (cdr move))
)

; Checks if two states are equivalent
; @param    move1   List containing first move
; @param    move2   List containing second move
; @return   T if the states are equivalent, nil otherwise
(defun same-state (move1 move2)
    (if (equal (get-state move1) (get-state move2))
        T
        nil
    )
)

; Returns a list of the directions taken based on an input list of moves
; @param    lst     List containing moves taken from last to first
; @return   A list of the moves taken from first to last
(defun path (lst)
    (cond
        ; Base case: if the direction from the current move is nil, return an empty list
        ((equal nil (get-direction (car lst))) '())
        ; Recursive case: build new list with the current direction appended to the recursive call for next move
        (T (append (path (cdr lst)) (list (get-direction (car lst)))))
    )
)

; Removes moves from a list if a move containing the same state appears in a second list
; @param    lst1    List of moves to check for redundant states
; @param    lst2    List of moves containing possible redundant states
; @return   A new list containing elements from the list 'lst1' with all redundancies from 'lst2' removed
(defun remove-redundant (lst1 lst2)
    (if (null lst1)
        ; Base case: at end of list 1, returns empty list
        '()
        ; Check if state of current move is in list 2
        (if (state-in-list (get-state (car lst1)) lst2)
            ; If T, skip move and check next in list 1
            (remove-redundant (cdr lst1) lst2)
            ; Else add move to return list and check next move
            (cons (car lst1) (remove-redundant (cdr lst1) lst2))
        )
    )
)
; Helper function for remove-redundant to check if a state is in a list
; @param    state   State to look for
; @param    lst     List to search through
; @return   T if state is found in list, nil otherwise
(defun state-in-list (state lst)
    (cond
        ; Base case: list is empty, return nil
        ((null lst) nil)
        ; If the given state is equal to the next state in the list, return T
        ((equal state (get-state (car lst))) T)
        ; Otherwise check the next state in the list
        (T (state-in-list state (cdr lst)))
    )
)

; Function to return the possible moves from a given state. Does not allow moves out-of-bounds
; position / 3 is the row of the empty spot
; position mod 3 is the col of the empty spot
; checks the row and col +/- 1 for valid moves, and if valid uses subst to move the empty spot to create a new move
; @param    state   Current state
; @return   A list of all possible moves from the given state
(defun moves (state)
    (remove-nil (cons (check-up state) (cons (check-down state) (cons (check-left state) (list (check-right state))))))
)
; Helper function to remove nil from a list
; @param    lst     List to remove all nil from
; @return   Copy of original list but with all nil removed
(defun remove-nil (lst)
    (cond
        ((null lst) nil) ; list is empty, return nil
        ((equal nil (car lst)) (remove-nil (cdr lst))) ; first is nil, return remove-nil for remainder of list
        (T (cons (car lst) (remove-nil (cdr lst)))) ; first isn't nil, return element & remove-nil for remainder
    )
)
; Helper function to check if U is a valid move
; @param    state   Current state
; @return   The resulting move if it is valid, nil otherwise
(defun check-up (state)
    (if (valid-spot (- (/ (position 'e state) 3) 1))
        (cons 'U (list (swap 'e (get-data (- (position 'e state) 3) state) state)))
        nil
    )
)
; Helper function to check if D is a valid move
; @param    state   Current state
; @return   The resulting move if it is valid, nil otherwise
(defun check-down (state)
    (if (valid-spot (+ (/ (position 'e state) 3) 1))
        (cons 'D (list (swap 'e (get-data (+ (position 'e state) 3) state) state)))
        nil
    )
)
; Helper function to check if L is a valid move
; @param    state   Current state
; @return   The resulting move if it is valid, nil otherwise
(defun check-left (state)
    (if (valid-spot (- (mod (position 'e state) 3) 1))
        (cons 'L (list (swap 'e (get-data (- (position 'e state) 1) state) state)))
        nil
    )
)
; Helper function to check if R is a valid move
; @param    state   Current state
; @return   The resulting move if it is valid, nil otherwise
(defun check-right (state)
    (if (valid-spot (+ (mod (position 'e state) 3) 1))
        (cons 'R (list (swap 'e (get-data (+ (position 'e state) 1) state) state)))
        nil
    )
)
; Helper function to return the data at a specified position in a list
; @param    pos     The position to check as an integer
; @param    lst     The list to check in
; @return   The data at the specified position of the given list
(defun get-data (pos lst)
    (if (equal pos 0)
        (car lst)
        (get-data (- pos 1) (cdr lst))
    )
)
; Helper function to swap two elements in a list
; @param    x       First element
; @param    y       Second element
; @param    lst     List to do the swap in
; @return   The resulting list after the swap is completed
(defun swap (x y lst)
    (subst y 'T (subst x y (subst 'T x lst)))
)
; Helper function to check if a given number is within the range of the puzzle matrix
; @param    x       Index to check
; @return   T if index is between 0 and 2 inclusive and nil otherwise
(defun valid-spot (x)
    (if (and (>= x 0) (< x 3))
        T
        nil
    )
)
; End of Lab 2 functions
;=============================================================
; Beginning of Lab 3

; Function 1
; Creates and initializes an open list (a list of paths) for the frontier
; @param    state       Initial state of the board
; @return   An open list starting at the provided state
(defun make-open-init (state)
    (list (list (cons nil (list state))))
)
;(make-open-init '(2 8 3 1 6 4 7 e 5)) results in (((NIL (2 8 3 1 6 4 7 E 5))))

; Function 2
; Creates a list of all possible extensions to a path with all redundant moves
; (ones that return to a previous state) removed
; @param    p           The path to extend
; @return   An open list of all possible extensions to the given path, minus
;           redundant moves
(defun extend-path (p)
    ; Get the state from p, then generate moves based on that, remove all redundancies
    ; from the moves that were in the original p, then send to create-path to
    ; turn the moves into paths
    (create-path (remove-redundant (moves (get-state (car p))) p) p '())
)
; Helper function to recursively build an open list using the moves generated
; by extend-path along with the original passed in path
; @param    moves       A list of moves generated by extend-path
; @param    origPath    The original path that was passed in to extend-path
; @param    openLst     New open list generated containing all paths after the extension
; @return   The openLst once complete
(defun create-path (moves origPath openLst)
    (if (null moves)
        ; Base case; return the open list
        openLst
        ; Recursive case; create path using current move & original path and send updated
        ; values to recursive call
        (create-path (cdr moves) origPath (append openLst (list (cons (car moves) origPath))))
    )
)
;(extend-path (car (make-open-init '(2 8 3 1 6 4 7 e 5)))) results in
;(((U (2 8 3 1 E 4 7 6 5)) (NIL (2 8 3 1 6 4 7 E 5))) ((L (2 8 3 1 6 4 E 7 5)) (NIL (2 8 3 1 6 4 7 E 5))) ((R (2 8 3 1 6 4 7 5 E)) (NIL (2 8 3 1 6 4 7 E 5))))

; Function 3
; Creates a path to the goal using a breadth-first search
; @param    openLst     The current open list containing paths to search
; @return   The path to the goal as returned by the path function
(defun search-bfs (openLst)
    (cond
        ; If the open list is empty, all possible paths have been exhausted and there is no solution
        ((null openLst) nil)
        ; Otherwise, check if the first move in the open list gets to the goal state. If so, return
        ; the path.
        ((goal-state (get-state(car (car openLst)))) (path (car openLst)))
        ; If not at the goal state, then extend the path from the first move and add those to the end
        ; of the open list
        (T (search-bfs (append (cdr openLst) (extend-path (car openLst)))))
    )
)
;(search-bfs (make-open-init '(2 8 3 1 6 4 7 e 5))) results in (U U L D R)

; Function 4
; Creates a path to the goal using a depth-first search with a depth bound
; @param    openLst     The current open list containing paths to search
; @param    limit       The depth bound to restrict the search to
; @return   The path to the goal as returned by the path function
(defun search-dfs-fd (openLst limit)
    (cond
        ; If the open list is empty, all possible paths have been exhausted and there is no solution
        ((null openLst) nil)
        ; Otherwise, check if the first move in the open list gets to the goal state. If so, return
        ; the path.
        ((goal-state (get-state(car (car openLst)))) (path (car openLst)))
        ; If not the goal state, check if the depth bound has been reached and if so, do not extend
        ; the node
        ((equal limit (- (length (car openLst)) 1)) (search-dfs-fd (cdr openLst) limit))
        ; Finally, the depth limit has not been reached so extend the next node and reduce the depth
        ; counter by one.
        (T (search-dfs-fd (append (extend-path (car openLst)) (cdr openLst)) limit))
    )
)
;(search-dfs-fd (make-open-init '(2 8 3 1 6 4 7 e 5)) 7) results in (U U L D R)

; Function 5
; Creates a path to the goal using an iterative deepening search algorithm
; @param    openLst     The current open list containing paths to search
; @return   The path to the goal as returned by the path function
(defun search-id (openLst)
    ; Begin the iterative deepening recursion starting at depth of 1
    (id-loop openLst 0)
)
; Helper function with an additional parameter to track current depth
(defun id-loop (openLst depth)
    (if (equal (search-dfs-fd openLst depth) nil)
        ; If a path is not found, increment the depth limit and call id-loop again 
        (id-loop openLst (+ depth 1))
        ; Otherwise, return the results from search-dfs-fd
        (search-dfs-fd openLst depth)
    )
)
;(search-id (make-open-init '(2 8 3 1 6 4 7 e 5))) results in (U U L D R)

; Function 6
; "State Space Search" - Provides a single function to run any of the 3 search algorithms
; implemented above.
; @param    state       The current state to build an open list and then search upon
; @param    type        (optional) Type of search to run (BFS, DFS, or ID). Defaults to BFS if omitted
; @param    depth       (optional) Depth of the DFS to run. Defaults to 7 if omitted.
; @return   The path to the goal as returned by the specified algorithm function
(defun sss (state &key type depth)
    (cond
        ; If BFS type given, run breadth first search
        ((equal type "BFS") (search-bfs (make-open-init state)))
        ; If DFS type given, check if depth given and run depth first search
        ((equal type "DFS") (if (equal depth nil)
            (search-dfs-fd (make-open-init state) 7)
            (search-dfs-fd (make-open-init state) depth))
        )
        ; If ID type given, run iterative deepening search
        ((equal type "ID") (search-id (make-open-init state)))
        (T (search-bfs (make-open-init state)))
    )
)
;(sss '(2 8 3 1 6 4 7 e 5) :type 'DFS :depth 14) results in (U U L D R)
;(sss '(2 8 3 1 6 4 7 e 5) :type 'DFS :depth 15) results in (U U L D R), but should result in
;   (U U L D D R U L D R …). This would be due to the DFS not finding the optimal path, but
;   the first one it comes across.
;(sss '(2 8 3 1 6 4 7 e 5) :type 'ID) results in (U U L D R)
;(sss '(2 8 1 E 6 3 7 5 4) :type 'DFS) results in (U R R D D L U L U R D)
;(sss '(3 1 2 e 8 4 7 6 5)) should result in (U R R D L L U R D R U L L D R) but still stack overflows
