breed [nodes node]
breed [binaryNodes binaryNode]
breed [cursors cursor]
breed [mouseCursors mouseCursor]
breed [nums num]

globals [
  Output
  mouse
  currState
  currentMoveIter
  currentEditIter
  currentCreaIter
  currentDelIter
  creaState
  currEditNode
  currCreaParent
  currCreaNode
  currCreaLink
  sideDetermined
  root
]

binaryNodes-own [
  data
  visualNum
  left_
  right_
  deleteAll
  setPos
  updateData
  connectLeft
  connectRight
  flash
  free
]

cursors-own [
  toggleVisibility
  storeSize
]

mouseCursors-own [
  pressedLast?
  previousDrag
  updatePos
  dragNodeHere
  updatePressed
]

nums-own [
  numberString
  numberTurtles
  updateNumTurts
  updateNum
  moveTurts
  free
]

; Currently unused
to-report cursors.new [x y cursSize cursColor showOnInit?]

  let toReport nobody;

  create-ordered-cursors 1 [

    set shape "line";
    set size cursSize;
    set color cursColor;
    setxy x y;
    set hidden? not showOnInit?

    ; Use to flash on and off when editing.
    set toggleVisibility [[] ->
      set hidden? not hidden?
    ]

    set toReport self;
  ]

  report toReport;

end

; Used to display numbers in place of netlogo's crappy text labels
to-report nums.new [posX posY number]
  let toReport nobody;

  create-ordered-nums 1 [
    ht;
    setxy posX posY;
    ; Actual numbers with shapes
    set numberTurtles (turtle-set);

    ; Changing the number.
    set updateNum [[newNum] ->
      set numberString (word newNum);
      run updateNumTurts;
    ]

    ; Moves the turtles without killing them
    set moveTurts [[x y] ->
      let toMoveX x - xcor;
      let toMoveY y - ycor;
      ask numberTurtles [
        setxy (xcor + toMoveX) (ycor + toMoveY);
      ]
      setxy x y;
    ]

    ; Updates the turtles only
    set updateNumTurts [[] ->
      ask numberTurtles [die];

      ; Determine the position of the middle or middle-right number
      let middle nobody;
      ifelse length numberString mod 2 = 0 [
        set middle (xcor + 0.5)
      ][
        set middle (xcor);
      ]

      ; Position each number individually, moving 1 right of the previous
      foreach (range length numberString) [[digPos] ->
        let toAdd nobody;
        hatch 1 [
          set breed turtles;
          let digit (item digPos [numberString] of myself);
          set shape digit;
          set xcor (digPos - floor (length [numberString] of myself / 2)) + middle;
          set size 2;
          set color white;
          st;

          set toAdd self;
        ]

        set numberTurtles (turtle-set numberTurtles toAdd);
      ]

    ]

    set free [[] ->
      ask numberTurtles [die];
      die;
    ]

    (run updateNum number);
    set toReport self;
  ]

  report toReport;
end

; The nodes that we are using - along with all the normal data stuff
to-report binaryNodes.new [visualX visualY visualSize nodeData leftNode rightNode]

  let toReport nobody;

  create-ordered-binaryNodes 1 [

    setxy visualX visualY;
    set size visualSize;
    set breed binaryNodes;
    set shape "circle";
    set data nodeData;

    ; Connection methods
    set connectLeft [[newLeft] ->
      set left_ newLeft;
      create-links-to (turtle-set left_) [
        set label "Left";
      ]
    ]

    set connectRight [[newRight] ->
      set right_ newRight;
      create-links-to (turtle-set right_) [
        set label "Right";
      ]
    ]

    (run connectLeft leftNode);
    (run connectRight rightNode);

    ; Utilize post-order bfs to delete the subtree.
    set deleteAll [[] ->
      if any? (turtle-set left_ right_) [
        ask (turtle-set left_ right_) [
          run deleteAll;
        ]
      ]
      run free;
    ]

    set flash [[toColor] ->
      set color toColor;
      wait 0.4;
      set color grey;
    ]

    set free [[] ->
      ask visualNum [
        run free;
      ]
      die;
    ]

    set toReport self;
  ]

  let temp nums.new [xcor] of toReport [ycor] of toReport [data] of toReport;

  ask toReport [
    set visualNum temp;

    ; Needed for moving numbers along
    set setPos [[x y] ->
      setxy x y;
      ask visualNum [
        (run moveTurts x y);
      ]
    ]

    set updateData [[newData] ->
      set data newData;
      ask visualNum [
        (run updateNum newData);
      ]
    ]
  ]

  report toReport;
end

; Only one should exist; used to help with in-radius functions, and to check
; presses vs holds, though there is currently no need for htat
to-report mouseCursors.new

  let toReport nobody;

  create-ordered-mouseCursors 1 [

    ht;
    set pressedLast? false;
    set previousDrag nobody;

    ; Move to mouse, should be consistently run
    set updatePos [[] ->
      setxy mouse-xcor mouse-ycor;
    ]

    set updatePressed [[] ->
      ifelse mouse-down? [
        set pressedLast? true;
      ][
        set pressedLast? false;
      ]
    ]

    ; Drags thenode to mouse cursor within radius 1.5; prioritizes previously held, then
    ; top to bottom.
    set dragNodeHere [[] ->
      ifelse mouse-down? and mouse-inside? [

        ifelse previousDrag != nobody [
          ask previousDrag [
            ; Move with boundaries on edges of screen
            (run setPos
              (min (list (15.5 - (count [numberTurtles] of visualNum / 2)) (max (list (-15.5 + (count [numberTurtles] of visualNum / 2)) [xcor] of myself))))
              (min (list 15.5 (max (list -15.5 [ycor] of myself))))
            );
          ]
        ][
          ; Check for new possible candidates
          if any? (binaryNodes in-radius 1.5) [
            set previousDrag max-one-of (binaryNodes in-radius 1.5) [who];
          ]
        ]

      ][

        set previousDrag nobody;

      ]
    ]

    set toReport self;
  ]

  report toReport;
end

to setup
  ca;
  reset-ticks;

  ; Initialise stuffs
  set mouse mouseCursors.new;
  set currState "none";
  set currEditNode nobody;
  set currCreaNode nobody;
  set currCreaParent nobody;
  set currCreaLink nobody;
  set sideDetermined nobody;
  set Output "";

  ; Hidden cursor that will move to number
  let s cursors.new 0 0 2 white false;

  ; Initial tree
  set root binaryNodes.new 0 10 2 "" nobody nobody;
end

to sample1
  ask root [run deleteAll];
  set root binaryNodes.new 0 10 2 12 (binaryNodes.new -4 6 2 7 nobody (binaryNodes.new -2 2 2 10 nobody nobody)) (binaryNodes.new 4 6 2 3 (binaryNodes.new 2 2 2 5 nobody nobody) (binaryNodes.new 6 2 2 6 nobody (binaryNodes.new 4 -2 2 1 nobody nobody)));
end

to sample2
  ask root [run deleteAll];
  set root binaryNodes.new 0 10 2 4 (binaryNodes.new -4 6 2 2 (binaryNodes.new -6 2 2 1 nobody nobody) (binaryNodes.new -2 2 2 3 nobody nobody)) (binaryNodes.new 4 6 2 5 nobody nobody)
end

to sample3
  ask root [run deleteAll];
  set root binaryNodes.new 0 10 2 "" nobody nobody;
end

to-report inorder
  let toOutput (list)

  let dfs nobody;
  set dfs [[bin_] ->
    if bin_ != nobody [
      ; Left, root, right
      (run dfs [left_] of bin_)
      ask bin_ [
        (run flash green)
      ]
      set toOutput lput ([data] of bin_) toOutput
      (run dfs [right_] of bin_)
    ]
  ]

  (run dfs root)
  report (word toOutput);
end

to-report preorder
  let toOutput (list)

  let dfs nobody;
  set dfs [[bin_] ->
    if bin_ != nobody [
      ; Root, left, right
      ask bin_ [
        (run flash green)
      ]
      set toOutput lput ([data] of bin_) toOutput
      (run dfs [left_] of bin_)
      (run dfs [right_] of bin_)
    ]
  ]

  (run dfs root)
  report (word toOutput);
end

to-report postorder
  let toOutput (list)

  let dfs nobody;
  set dfs [[bin_] ->
    if bin_ != nobody [
      ; Left, right, root
      (run dfs [left_] of bin_)
      (run dfs [right_] of bin_)
      ask bin_ [
        (run flash green)
      ]
      set toOutput lput ([data] of bin_) toOutput
    ]
  ]

  (run dfs root)
  report (word toOutput);
end

to-report levelorder
  let toOutput (list root)
  let bfs nobody;

  set bfs [[binList] ->

    if not empty? binList [

      let nextLevel (list)
      foreach binList [x ->
        if [left_] of x != nobody [
          set nextLevel lput [left_] of x nextLevel;
          ask [left_] of x [
            (run flash green)
          ]
        ]
        if [right_] of x != nobody [
          set nextLevel lput [right_] of x nextLevel;
          ask [right_] of x [
            (run flash green)
          ]
        ]
      ]

      set toOutput (sentence toOutput nextLevel)

      (run bfs nextLevel)
    ]

  ]

  ask root [
    (run flash green)
  ]
  (run bfs (list root))

  report (word map [i -> [data] of i] toOutput);
end

to-report findNodeHeight [bin_]
  let toReport 0;
  if bin_ != nobody [
    ask bin_ [
      (run flash green)
    ]
    set toReport max (list (findNodeHeight [left_] of bin_ + 1) (findNodeHeight [right_] of bin_ + 1));
  ]
  report toReport;
end

to convertSearchTree
  let tempIn sort read-from-string inorder;
  set Output tempIn;

  wait 0.5;
  ; Basically replicate with inorder
  let conversion nobody;
  set conversion [[bin_] ->
    if bin_ != nobody [
      (run conversion [left_] of bin_)
      ask bin_ [
        (run updateData first tempIn)
        set tempIn but-first tempIn
        (run flash yellow);
      ]
      (run conversion [right_] of bin_)
    ]
  ]
  (run conversion root)
end

to-report findRange
  convertSearchTree

  let leftMost [left_] of root
  let rightMost [right_] of root

  while [[left_] of leftMost != nobody] [
    set leftMost [left_] of leftMost
    ask leftMost [
      (run flash green)
    ]
  ]
  while [[right_] of rightMost != nobody] [
    set rightMost [right_] of rightMost
    ask rightMost [
      (run flash green)
    ]
  ]

  report [data] of rightMost - [data] of leftMost;
end

to runAlg
  set Output "";
  set currState "none";

  if algorithm = "Inorder traversal" [
    set Output inorder;
  ]

  if algorithm = "Preorder traversal" [
    set Output preorder
  ]

  if algorithm = "Postorder traversal" [
    set Output postorder
  ]

  if algorithm = "Level order traversal" [
    set Output levelorder
  ]

  if algorithm = "Find tree height" [
    set Output (findNodeHeight root);
  ]

  if algorithm = "Convert to search tree" [
    convertSearchTree
  ]

  if algorithm = "Find range of data in a tree" [
    set Output findRange
  ]
end


to moveNodes
  ; Cancel all other states on first iteration
  if currentMoveIter = 0 [
    set currState "move";
    set currentMoveIter 1;
  ]

  ifelse currState = "move" [

    ask mouse [
      run updatePos;
      run dragNodeHere;
    ]

  ][

    set currentMoveIter 0;
    stop;

  ]
end

to input [toInput]
  if currEditNode != nobody [

    ifelse toInput != "enter" [
      ask currEditNode [

        ask visualNum [
          ifelse toInput = "del" [
            if length numberString > 0 [
              (run updateNum but-last numberString);
            ]
            ask cursors [
              set xcor (xcor - 0.5);
            ]
          ][
            ifelse length numberString < 3 [
              (run updateNum (word numberString toInput));
            ][
              ask patch 16 16 [
                set plabel "Too large!"
              ]
            ]
            ask cursors [
              set xcor (xcor + 0.5);
            ]
          ]
        ]
        ask currEditNode [
          set data read-from-string [numberString] of visualNum;
        ]

      ]

    ][
      set currEditNode nobody;
      ask cursors [die];
    ]
  ]
end

to editNode
  ; Stop all others
  if currentEditIter = 0 [
    set currState "edit";
    set currentEditIter 1;
    ask patch 16 16 [
      set plabel "Cannot delete the root node!"
    ]
  ]
  ifelse currState = "edit" [

    ask mouse [
      run updatePos;
      ; There is no need to use pressed, as the current node will always be
      ; on top because there is no asynchrous linking
      if mouse-down? and mouse-inside? [
        ifelse any? (binaryNodes in-radius 1.5)[
          ask max-one-of (binaryNodes in-radius 1.5) [who] [
            set currEditNode self;
          ]

          reset-timer;
        ][
          ask cursors [ht];
          set currEditNode nobody;
        ]
      ]

    ]

    if currEditNode != nobody [
      let cursorPos [xcor] of [visualNum] of currEditNode;
      if any? ([numberTurtles] of [visualNum] of currEditNode) [
        set cursorPos [xcor] of max-one-of ([numberTurtles] of [visualNum] of currEditNode) [xcor] + 0.5;
      ]

      ask cursors [
        setxy (cursorPos) ([ycor] of [visualNum] of currEditNode);
        if timer <= 0.3 and timer >= 0.25 [
          run toggleVisibility;
          reset-timer;
        ]
      ]

    ]

  ][
    ask cursors [ht];
    set currentEditIter 0;
    stop;
  ]
end

to newNode
  if currentCreaIter = 0 [
    set currState "crea";
    set currentCreaIter 1;
    ask patch 16 16 [
      set plabel "Drag from a node to extend from";
    ]
  ]
  ifelse currState = "crea" [

    ask mouse [
      run updatePos;
    ]
    if mouse-inside? [
      ifelse mouse-down? [

        ifelse currCreaNode = nobody [

          if any? ([binaryNodes in-radius 1.5] of mouse)[

            set currCreaParent [max-one-of (binaryNodes in-radius 1.5) [who]] of mouse;

            ifelse count (turtle-set [left_] of currCreaParent [right_] of currCreaParent) = 2 [

              ask patch 16 16 [
                set plabel "That node already has two children!";
              ]

              set currCreaParent nobody;

            ][
              ask patch 16 16 [
                set plabel "Drag from a node to extend from";
              ]

              set currCreaNode binaryNodes.new ([xcor] of currCreaParent) ([ycor] of currCreaParent) 2 "" nobody nobody

              if [left_] of currCreaParent != nobody [
                set sideDetermined "Right"
              ]
              if [right_] of currCreaParent != nobody [
                set sideDetermined "Left"
              ]
              ask currCreaParent [
                create-link-to currCreaNode [
                  set label sideDetermined;
                  set currCreaLink self;
                ]
              ]
            ]

          ]

        ][

          ask mouse [
            set previousDrag currCreaNode;
            if sideDetermined = nobody [
              ask currCreaLink [
                ifelse [xcor] of currCreaNode < [xcor] of currCreaParent [
                  set label "Left"
                ][
                  set label "Right";
                ]
              ]
            ]
            run dragNodeHere;
          ]

        ]

      ][

        ; On release
        if currCreaLink != nobody and [label] of currCreaLink = "Right" [
          ask currCreaParent [
            set right_ currCreaNode;
          ]
        ]
        if currCreaLink != nobody and [label] of currCreaLink = "Left" [
          ask currCreaParent [
            set left_ currCreaNode;
          ]
        ]
        set currCreaParent nobody;
        set currCreaLink nobody;
        set sideDetermined nobody;
        set currCreaNode nobody;

      ]

    ]
  ][
    ask patch 16 16 [
      set plabel "";
    ]
    set currentCreaIter 0;
    stop;
  ]
end

to deleteNode
  if currentDelIter = 0 [
    set currState "del";
    set currentDelIter 1;
    ask patch 16 16 [
      set plabel "Click on the node you want to delete";
    ]
  ]
  ifelse currState = "del" [
    ask mouse [
      (run updatePos)
      if mouse-down? and mouse-inside? [
        if any? (binaryNodes in-radius 1.5)[
          ask max-one-of (binaryNodes in-radius 1.5) [who] [
            ifelse self = root [
              ask patch 16 16 [
                set plabel "Cannot delete the root node!"
              ]
            ][
              ask patch 16 16 [
                set plabel "Click on the node you want to delete";
              ]
              run deleteAll

            ]
          ]
        ]
      ]
    ]
  ][
    ask patch 16 16 [
      set plabel "";
    ]
    set currentDelIter 0;
    stop;
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
342
10
779
448
-1
-1
13.0
1
16
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
60.0

CHOOSER
21
117
290
162
algorithm
algorithm
"Level order traversal" "Inorder traversal" "Preorder traversal" "Postorder traversal" "Find tree height" "Convert to search tree" "Find range of data in a tree"
6

BUTTON
37
26
100
59
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
187
461
296
494
Move nodes
moveNodes
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
187
397
296
430
Edit a node
editNode
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
187
365
296
398
Create new node
newNode
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
21
211
175
244
Run the algorithm!
runAlg
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
12
364
67
397
1
input 1
NIL
1
T
OBSERVER
NIL
1
NIL
NIL
1

BUTTON
67
364
122
397
2
input 2
NIL
1
T
OBSERVER
NIL
2
NIL
NIL
1

BUTTON
122
364
177
397
3
input 3
NIL
1
T
OBSERVER
NIL
3
NIL
NIL
1

BUTTON
12
396
67
429
4
input 4
NIL
1
T
OBSERVER
NIL
4
NIL
NIL
1

BUTTON
67
396
122
429
5
input 5
NIL
1
T
OBSERVER
NIL
5
NIL
NIL
1

BUTTON
122
396
177
429
6
input 6
NIL
1
T
OBSERVER
NIL
6
NIL
NIL
1

BUTTON
12
428
67
461
7
input 7
NIL
1
T
OBSERVER
NIL
7
NIL
NIL
1

BUTTON
67
428
122
461
8
input 8
NIL
1
T
OBSERVER
NIL
8
NIL
NIL
1

BUTTON
122
428
177
461
9
input 9
NIL
1
T
OBSERVER
NIL
9
NIL
NIL
1

BUTTON
67
461
122
494
0
input 0
NIL
1
T
OBSERVER
NIL
0
NIL
NIL
1

BUTTON
12
461
67
494
Delete
input \"del\"
NIL
1
T
OBSERVER
NIL
P
NIL
NIL
1

BUTTON
122
461
177
494
Enter
input \"enter\"
NIL
1
T
OBSERVER
NIL
;
NIL
NIL
1

BUTTON
187
429
296
462
Delete a node
deleteNode
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
818
10
968
30
Sample Trees
16
0.0
1

BUTTON
816
70
911
103
Just A Tree
sample1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
816
102
946
135
Already Binary Tree
sample2
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
19
328
169
348
Tree Editing
16
0.0
1

TEXTBOX
24
91
217
131
Pick an algorithm to run
16
0.0
1

MONITOR
21
162
293
211
NIL
Output
17
1
12

TEXTBOX
955
103
1105
131
<- try running inorder traversal on this!
11
0.0
1

BUTTON
816
38
909
71
Empty tree
sample3
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

0
false
0
Line -7500403 true 105 105 120 75
Line -7500403 true 120 75 150 60
Line -7500403 true 150 60 180 75
Line -7500403 true 180 75 195 105
Line -7500403 true 105 105 105 195
Line -7500403 true 195 105 195 195
Line -7500403 true 105 195 120 225
Line -7500403 true 120 225 150 240
Line -7500403 true 150 240 180 225
Line -7500403 true 180 225 195 195

1
false
0
Line -7500403 true 105 90 150 60
Line -7500403 true 150 60 150 240
Line -7500403 true 105 240 195 240

2
false
0
Line -7500403 true 105 240 195 240
Line -7500403 true 105 240 195 135
Line -7500403 true 195 135 195 105
Line -7500403 true 195 105 180 75
Line -7500403 true 180 75 150 60
Line -7500403 true 150 60 120 75
Line -7500403 true 120 75 105 105

3
false
0
Line -7500403 true 105 105 120 75
Line -7500403 true 120 75 150 60
Line -7500403 true 150 60 180 75
Line -7500403 true 180 75 195 105
Line -7500403 true 195 105 180 135
Line -7500403 true 180 135 150 150
Line -7500403 true 150 150 180 165
Line -7500403 true 180 165 195 195
Line -7500403 true 195 195 180 225
Line -7500403 true 180 225 150 240
Line -7500403 true 150 240 120 225
Line -7500403 true 120 225 105 195

4
false
0
Line -7500403 true 150 60 105 150
Line -7500403 true 105 150 195 150
Line -7500403 true 150 60 150 240

5
false
0
Line -7500403 true 180 60 105 60
Line -7500403 true 105 60 105 135
Line -7500403 true 105 225 150 240
Line -7500403 true 150 240 180 225
Line -7500403 true 180 225 195 180
Line -7500403 true 195 180 180 150
Line -7500403 true 180 150 150 135
Line -7500403 true 105 135 150 135

6
false
0
Line -7500403 true 120 225 150 240
Line -7500403 true 150 240 180 225
Line -7500403 true 120 225 105 195
Line -7500403 true 105 195 120 165
Line -7500403 true 120 165 150 150
Line -7500403 true 150 150 180 165
Line -7500403 true 180 165 195 195
Line -7500403 true 195 195 180 225
Line -7500403 true 105 135 105 195
Line -7500403 true 105 135 120 90
Line -7500403 true 120 90 135 75
Line -7500403 true 135 75 165 60
Line -7500403 true 165 60 180 60

7
false
0
Line -7500403 true 105 60 195 60
Line -7500403 true 195 60 105 240

8
false
0
Line -7500403 true 150 60 120 75
Line -7500403 true 120 75 105 105
Line -7500403 true 105 105 120 135
Line -7500403 true 120 135 150 150
Line -7500403 true 150 150 120 165
Line -7500403 true 180 135 150 150
Line -7500403 true 150 150 180 165
Line -7500403 true 150 60 180 75
Line -7500403 true 120 225 150 240
Line -7500403 true 180 225 150 240
Line -7500403 true 195 105 180 135
Line -7500403 true 180 75 195 105
Line -7500403 true 180 165 195 195
Line -7500403 true 105 195 120 225
Line -7500403 true 120 165 105 195
Line -7500403 true 195 195 180 225

9
false
0
Line -7500403 true 120 75 150 60
Line -7500403 true 150 60 180 75
Line -7500403 true 180 75 195 105
Line -7500403 true 120 75 105 105
Line -7500403 true 105 105 120 135
Line -7500403 true 120 135 150 150
Line -7500403 true 150 150 180 135
Line -7500403 true 180 135 195 105
Line -7500403 true 195 105 195 165
Line -7500403 true 195 165 180 210
Line -7500403 true 180 210 165 225
Line -7500403 true 165 225 135 240
Line -7500403 true 120 240 135 240

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
