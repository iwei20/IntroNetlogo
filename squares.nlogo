breed [objects object]
breed [squares square]
breed [dialogues dialogue]
breed [textLabels textLabel]
breed [thoughtBubbles thoughtBubble]
breed [trajs traj]
breed [marios mario]
breed [gooms goom]
extensions [sound]
squares-own [mSideLength]
dialogues-own [mName mSide mText nameBox textBox namePointer textPointer1 textPointer2]
textLabels-own [mText currentIndex]
thoughtBubbles-own [bubble1 bubble2 cloud]
trajs-own [trajList previewCircle previewPos]
marios-own [fallSpeed];
gooms-own [fallSpeed falling?];

;;; OBJECTS ;;;
to-report objects._init_
  let toReport nobody;
  create-ordered-objects 1 [
    set toReport self;
  ]
  report toReport;
end

to objects.setPosition [mObject xPos yPos]
  ask mObject [
    setxy xPos yPos;
  ]
end

to objects.move [mObject xShift yShift]
  objects.setPosition mObject ([xcor] of mObject + xShift) ([ycor] of mObject + yShift);
end

to objects.rot [mObject degrees]
  ask mObject [
    rt degrees;
  ]
end

to objects.free [mObject]
  ask mObject [
    die;
  ]
end

;;; SQUARES ;;;
to-report squares._init_ [sideLength toColor]
  let toReport objects._init_
  ask toReport [
    set breed squares;
    set shape "empty";
    set mSideLength sideLength;
    set color toColor;
    set pen-size 2;
  ]
  squares.draw toReport;
  report toReport;
end

to squares.setSidelength [mSquare sideLength]
  ask mSquare [
    set mSideLength sideLength;
  ]
end

to squares.addSize [mSquare lengthToAdd]
  squares.setSideLength mSquare ([mSideLength] of mSquare + lengthToAdd)
end

to squares.draw [mSquare]
  ask mSquare [
    fd mSideLength / 2;
    rt 90;
    repeat 4 [
      pd;
      fd mSideLength / 2;
      rt 90;
      fd mSideLength / 2;
      pu;
    ]
    rt 90;
    fd mSideLength / 2;
  ]
end

;;; BLUE SQUARES ;;;
to-report blueSquares._init_ [sideLength]
  report squares._init_ sideLength blue;
end

;;; DIALOGUE ;;;
to-report dialogues._init_ [name side mColor]
  let toReport objects._init_;
  let mNameBox objects._init_;
  let mTextBox objects._init_;
  let sign 0;
  ifelse side = "right" [
    set sign 1;
  ][
    set sign -1;
  ]
  let mNamePointer textLabels._init_ name true ((sign * 11.25) + 3.75) -5;
  let mTextPointer1 textLabels._init_ "" false 15 -9;
  let mTextPointer2 textLabels._init_ "" false 15 -11;

  ask toReport [
    set breed dialogues;
    set shape "empty";
    set nameBox mNameBox;
    set textBox mTextBox;
    set namePointer mNamePointer
    set textPointer1 mTextPointer1;
    set textPointer2 mTextPointer2;

    ask nameBox [
      set shape "textbox";
      set color mColor;
      set size 10;
      setxy sign * 11.25 -5;
    ]
    ask textBox [
      set shape "textbox";
      set color mColor;
      set size 33;
      setxy 0 -11.5;
    ]
  ]
  report toReport;
end

to dialogues.changeSide [dialogueObject side]
  ask dialogueObject [
    ifelse side = "right" [
      setxy 11.25 -5;
    ][
      setxy -11.25 -5;
    ]
  ]
end

to dialogues.putText1 [dialogueObject text]
  ask dialogueObject [
    textLabels.addText textPointer1 text;
  ]
end

to dialogues.putText2 [dialogueObject text]
  ask dialogueObject [
    textLabels.addText textPointer2 text;
  ]
end

to dialogues.clearText [dialogueObject]
  ask dialogueObject [
    textLabels.clearText textPointer1;
    textLabels.clearText textPointer2;
  ]
end

to dialogues.showNextCharacter [dialogueObject]
  ask dialogueObject [
    ifelse textLabels.hasNext textPointer1 [
      textLabels.showNextNLetters textPointer1 1;
    ][
      textLabels.showNextNLetters textPointer2 1;
    ]
  ]
end

to-report dialogues.hasNext [dialogueObject]
  report textLabels.hasNext [textPointer1] of dialogueObject or textLabels.hasNext [textPointer2] of dialogueObject;
end

to dialogues.free [dialogueObject]
  ask dialogueObject [
    objects.free nameBox;
    objects.free textBox;
    objects.free namePointer;
    objects.free textPointer1;
    objects.free textPointer2;
  ]
  objects.free dialogueObject;
end

to-report textLabels._init_ [text showOnCreation? x y]
  let toReport objects._init_;
  ask toReport [
    set breed textLabels;
    objects.setPosition self x y;
    set shape "empty";
    set mText text;
    if showOnCreation? [
      set label mText;
    ]
  ]
  report toReport;
end

to textLabels.setText [mTextLabel newText]
  ask mTextLabel [
    set mText newText;
  ]
end

to textLabels.instantShowText [mTextLabel]
  ask mTextLabel [
    set label mText;
    set currentIndex (length mText)
  ]
end

to textLabels.showNextNLetters [mTextLabel n]
  ask mTextLabel [
    set label (word label (substring mText currentIndex (currentIndex + n)));
    set currentIndex (currentIndex + n)
  ]
end

to textLabels.addText [mTextLabel text]
  ask mTextLabel [
    set mText (word mText text);
  ]
end

to textLabels.clearText [mTextLabel]
  ask mTextLabel [
    set mText "";
    set label "";
    set currentIndex 0;
  ]
end

to textLabels.setColor [mTextLabel newColor]
  ask mTextLabel [
    set label-color newColor;
  ]
end

to-report textLabels.hasNext [mTextLabel]
  report [currentIndex] of mTextLabel < length [mText] of mTextLabel;
end

;;; PEOPLE ;;;
to-report person._init_ [name mShape mSize]
  let toReport objects._init_;
  ask toReport [
    set size mSize;
    set shape mShape;
    set label name;
  ]
  report toReport;
end

to person.updateShape [person mShape]
  ask person [
    set shape mShape;
  ]
end

;;; THOUGHT BUBBLE ;;;
to-report thoughtBubbles._init_
  let toReport objects._init_;
  let mbubble1 objects._init_;
  let mbubble2 objects._init_;
  let mcloud objects._init_;
  ask toReport [
    ht;
    set breed thoughtBubbles;
    set bubble1 mbubble1;
    set bubble2 mbubble2;
    set cloud mcloud;

    ask bubble1 [
      ht
      set shape "circle";
      set size 4;
      setxy -13 -13;
    ]
    ask bubble2 [
      ht
      set shape "circle";
      set size 4;
      setxy -7 -8;
    ]
    ask cloud [
      ht
      set shape "cloud";
      set size 25;
      setxy 3 5;
    ]
  ]
  report toReport
end

to thoughtBubbles.pop1 [thoughtBubbleObject rate/tick]
  let lines turtle-set nobody;
  ask thoughtBubbleObject [
    ask bubble1 [
      st;
    ]
  ]

  cro 8 [
    set shape "line half";
    set lines (turtle-set self lines);
    set color [color] of [bubble1] of thoughtBubbleObject;
    move-to [bubble1] of thoughtBubbleObject;
  ]

  sound:play-drum "Mute Hi Conga" 64
  foreach (range 7) [
    ask lines [fd rate/tick];
    tick;
  ]
  ask lines [die]
  tick;
end


to thoughtBubbles.pop2 [thoughtBubbleObject rate/tick]
  let lines turtle-set nobody;
  ask thoughtBubbleObject [
    ask bubble2 [
      st;
    ]
  ]

  cro 8 [
    set shape "line half";
    set lines (turtle-set self lines);
    set color [color] of [bubble2] of thoughtBubbleObject;
    move-to [bubble2] of thoughtBubbleObject;
  ]

  sound:play-drum "Mute Hi Conga" 64
  foreach (range 7) [
    ask lines [fd rate/tick];
    tick;
  ]
  ask lines [die]
  tick;
end

to thoughtBubbles.popCloud [thoughtBubbleObject rate/tick]
  let lines turtle-set nobody;
  ask thoughtBubbleObject [
    ask cloud [
      st;
    ]
  ]

  cro 8 [
    set shape "line half";
    set lines (turtle-set self lines);
    set color [color] of [cloud] of thoughtBubbleObject;
    move-to [cloud] of thoughtBubbleObject;
    fd 8
  ]

  sound:play-drum "Mute Hi Conga" 64
  foreach (range 7) [
    ask lines [fd rate/tick];
    tick;
  ]
  ask lines [die]
  tick;
end

to-report horLines._init_ [yPos]
  let toReport objects._init_;
  ask toReport [
    ht;
    setxy -16.5 yPos;
    rt 90;
  ]
  report toReport;
end

to horLines.draw [horizontalLine]
  ask horizontalLine [
    pd;
    fd world-width;
    pu;
    bk world-width;
  ]
end

to horLines.setY [horizontalLine yPos]
  ask horizontalLine [
    set ycor yPos;
  ]
end

to horLines.setThickness [horizontalLine thick]
  ask horizontalLine [
    set pen-size thick;
  ]
end

to horLines.setColor [horizontalLine colour]
  ask horizontalLine [
    set color colour;
  ]
end

to-report trajs._init_ [x1 y1 x2 y2 a]
  let toReport objects._init_;
  let initCircle objects._init_;

  ask toReport [
    ht;
    set breed trajs;
    trajs.updateTraj self x1 y1 x2 y2 a;
  ]
  ask initCircle [
    ht;
    set shape "circle";
    set size 0.3;
    setxy (item 0 (item 0 [trajList] of toReport)) (item 1 (item 0 [trajList] of toReport));
    stamp;
  ]

  ask toReport [
    set previewCircle initCircle;
  ]

  report toReport;
end

to trajs.updateTraj [trajObj x1 y1 x2 y2 a]
  ask trajObj [
    let b (a * x2 ^ 2 - a * x1 ^ 2 + y1 - y2) / (x1 - x2);
    let c y1 - a * x1 ^ 2 - b * x1;

    set trajList (list);
    let i x1;
    while [i <= x2] [
      set trajList lput (list precision i 2 (a * i ^ 2 + b * i + c)) trajList;
      set i precision (i + 0.1) 2;
    ]
  ]
end

to trajs.preview [trajObj]
  ask trajObj [
    foreach (filter [coord -> position coord trajList mod 10 = previewPos] trajList) [ n ->
      ask previewCircle [
        setxy (item 0 n) (item 1 n);
        stamp;
      ]
    ]
    set previewPos (previewPos + 1) mod 10;
  ]
end

to intro
  sound:play-drum "Mute Hi Conga" 64
  let title textLabels._init_ "Blue Square Ideas" true 5 2
  tick;
  wait 0.5;
  sound:play-drum "Mute Hi Conga" 64
  let author textLabels._init_ "By Ivan Wei" true 3 0
  tick;
  wait 2;
  ct;
end

to mrbrooksteaches
  ; Initial scene: Mr. Brooks, seen teaching how to
  let tvscreen objects._init_;
  let brooks person._init_ "" "mrbrooks" 18;
  objects.setPosition brooks 10 0;
  ask tvscreen [
    set shape "tvscreen";
    setxy -6 7;
    set size 20;
  ]

  let dialog dialogues._init_ "Mr. Brooks" "right" blue;

  dialogues.putText1 dialog "Alright, class, here comes your first challenge."
  dialogues.putText2 dialog "Draw a blue square like this!"
  while [dialogues.hasNext dialog] [
    dialogues.showNextCharacter dialog
    sound:play-note "TRUMPET" 58 64 0.1;
    tick;
  ]

  ; Mr. Brooks points and shows a blue square on screen.
  wait 0.5;
  person.updateShape brooks "mrbrookspointing";
  let blueSquare blueSquares._init_ 3;
  cd;
  objects.setPosition blueSquare -6 7;
  squares.draw blueSquare;
  tick;
  wait 0.5;

  dialogues.clearText dialog;
  dialogues.putText1 dialog "Remember, you can use repeat to make your life easier!";
  while [dialogues.hasNext dialog] [
    dialogues.showNextCharacter dialog
    sound:play-note "TRUMPET" 58 64 0.1;
    tick;
  ]
  wait 2;

  objects.free brooks
  objects.free tvscreen;
  dialogues.free dialog;
  cd;

  ; Scene pans to Ivan, who looks at it and thinks its interesting.
  let computerdesk objects._init_;
  ask computerdesk [
    set shape "computertable";
    setxy -5 0
    set size 20;
  ]

  let ivan person._init_ "" "ivan" 25;
  objects.setPosition ivan 7 0;
  set dialog dialogues._init_ "Ivan(Me)" "left" blue;
  dialogues.putText1 dialog "Wow! This seems so interesting!";
  dialogues.putText2 dialog "Hmm,"
  while [dialogues.hasNext dialog] [
    dialogues.showNextCharacter dialog
    sound:play-note "TRUMPET" 60 64 0.1;
    tick;
  ]

  ; Ivan thinks up a solution!
  wait 1;
  dialogues.clearText dialog;
  dialogues.putText1 dialog "Lets try, cro 1 [set color blue pd repeat 4 [fd 1 rt 90]]"
  while [dialogues.hasNext dialog] [
    dialogues.showNextCharacter dialog
    sound:play-note "TRUMPET" 60 64 0.1;
    tick;
  ]
  wait 1;

  dialogues.free dialog;
  objects.free ivan;
  objects.free computerdesk;

  ; Ivan types the command in
  let commandcenter objects._init_;
  ask commandcenter [
    set shape "commandcenter";
    set size 33;
    set ycor -11.5;
  ]

  let consoleInput textLabels._init_ "observer> cro 1 [set color blue pd repeat 4 [fd 5 rt 90]]" true 16 -15;
  textLabels.setColor consoleInput black;
  tick;
  wait 1;
  sound:play-drum "Hi Wood Block" 64
  objects.move consoleInput -2.25 2;
  tick;
  wait 1;

  ; Meta square drawing haha fourth wall
  cro 1 [
    set color blue;
    set pen-size 2;
    pd
    repeat 4 [
      fd 5;
      rt 90;
    ]
  ]
  sound:play-drum "Low Wood Block" 64
  tick;
  wait 1.5;

  ; Ivan views his newly drawn square with excitement, and begins to imagine what he could do.
  textLabels.clearText consoleInput;
  set dialog dialogues._init_ "Ivan(Me)" "right" blue;
  dialogues.putText1 dialog "Wow!..."
  while [dialogues.hasNext dialog] [
    dialogues.showNextCharacter dialog
    sound:play-note "TRUMPET" 60 64 0.1;
    tick;
  ]

  wait 0.5;
  dialogues.putText2 dialog "I think can make games in NetLogo now!"
  while [dialogues.hasNext dialog] [
    dialogues.showNextCharacter dialog
    sound:play-note "TRUMPET" 60 64 0.1;
    tick;
  ]

  wait 1;
  ; The scene starts to trip out, going to different ideas. Transition by swirling everything.
  let placeboSquare blueSquares._init_ 5;
  cd;
  dialogues.clearText dialog;
  objects.setPosition placeboSquare 2.5 2.5;

  let pitch 56;
  while [[patch-here] of [textBox] of dialog != patch 0 0] [
    cd;
    every 0.1 [
      sound:play-note "rain" pitch 64 0.1;
      set pitch (pitch + 1)
    ]
    squares.draw placeboSquare;
    squares.setSidelength placeboSquare (0.99 * [mSidelength] of placeboSquare);
    ask turtles [
      face patch 0 0;
      fd 0.1;
      set size (size * 0.99)
      let cos1 cos 1;
      let sin1 sin 1;
      setxy (cos1 * xcor - sin1 * ycor) (sin1 * xcor + cos1 * ycor);
    ]
    tick;
  ]
  cd;
  ct;
  let bubble thoughtBubbles._init_;
  thoughtBubbles.pop1 bubble 0.5
  wait 0.2;
  thoughtBubbles.pop2 bubble 0.5
  wait 0.2;
  thoughtBubbles.popCloud bubble 0.5;
  wait 0.2;
  while [[size] of [cloud] of bubble < 65] [
    ask [cloud] of bubble [
      set size (size + 0.5);
    ]
    every 0.1 [
      sound:play-note "rain" pitch 64 0.1;
      ifelse pitch = 93 [
        set pitch 92
      ][
        set pitch 93
      ]
    ]
    tick;
  ]
  wait 1;
  ct;
end

to toddRogers
  ; The background
  ask patches [
    set pcolor green;
  ]

  ; My car racer !
  let s blueSquares._init_ 5;
  let s2 blueSquares._init_ 5;
  objects.setPosition s -10 9;
  objects.setPosition s2 -10 -5.5;
  cd;

  ; Background
  let player1Back objects._init_;
  ask player1Back [
    set shape "toddrogersback";
    set size 33;
    set ycor 13;
  ]

  ; Front, gui with time and gear
  let player1FrontTop horLines._init_ 4;
  let player1FrontBot horLines._init_ 2;
  horLines.setColor player1FrontTop black;
  horLines.setColor player1FrontBot black;

  let player1FrontTime textLabels._init_ "10" true -8 3;
  let player1FrontGear textLabels._init_ "N" true 1 3;
  textLabels.setColor player1FrontTime black;
  textLabels.setColor player1FrontGear black;

  let player2Back objects._init_;
  ask player2Back [
    set shape "toddrogersback";
    set size 33;
    set ycor -1.5;
  ]
  let player2FrontTop horLines._init_ -10.5;
  let player2FrontBot horLines._init_ -12.5;
  horLines.setColor player2FrontTop black;
  horLines.setColor player2FrontBot black;

  let player2FrontTime textLabels._init_ "10" true -8 -11.5;
  let player2FrontGear textLabels._init_ "N" true 1 -11.5;
  textLabels.setColor player2FrontTime black;
  textLabels.setColor player2FrontGear black;

  let logo textLabels._init_ "Activision" true -8 -13;
  textLabels.setColor logo black;

  squares.draw s;
  horLines.draw player1FrontTop;
  horLines.draw player1FrontBot;
  squares.draw s2;
  horLines.draw player2FrontTop;
  horLines.draw player2FrontBot;
  tick;

  ; Race countdown
  let rotSpeed 10;
  repeat 10 [
    textLabels.setText player1FrontTime (word (read-from-string [mText] of player1FrontTime - 1));
    textLabels.setText player2FrontTime [mText] of player1FrontTime;
    textLabels.instantShowText player1FrontTime;
    textLabels.instantShowText player2FrontTime;
    sound:play-note "TRUMPET" 55 64 0.1;
    wait 0.25;
    tick;
  ]

  ; Race start!
  reset-timer;

  let counter 0;
  repeat 161 [
    cd
    objects.move s 0.12 0;
    every 1 [
      ifelse counter < 4 [
        sound:play-note "trumpet" 59 64 0.1;
        sound:play-note-later 0.15 "trumpet" 59 64 0.1
        set counter (counter + 1);
      ][
        if counter = 4 [
          sound:play-note "trumpet" 45 64 0.5
          sound:play-note-later 0.2 "trumpet" 53 64 0.5;
          sound:play-note-later 0.4 "trumpet" 55 64 0.5;
          sound:play-note-later 0.6 "trumpet" 57 64 0.5;
          sound:play-note-later 0.8 "trumpet" 60 64 0.5;
          sound:play-note-later 1 "trumpet" 62 64 0.5;
        ]
        set counter (counter + 1);
      ]
    ]
    objects.rot s rotSpeed;
    set rotSpeed (rotSpeed + 0.3);

    let nextTime (word precision timer 2);
    foreach (range (length nextTime) 4) [
      set nextTime (word nextTime "0");
    ]

    textLabels.setText player1FrontTime nextTime;
    textLabels.setText player2FrontTime [mText] of player1FrontTime;
    textLabels.instantShowText player1FrontTime;
    textLabels.instantShowText player2FrontTime;

    squares.draw s;
    horLines.draw player1FrontTop;
    horLines.draw player1FrontBot;
    squares.draw s2;
    horLines.draw player2FrontTop;
    horLines.draw player2FrontBot;
    tick;
  ]

  textLabels.setText player1FrontGear "C";
  textLabels.instantShowText player1FrontGear;
  textLabels.setText player1FrontTime "5.57";
  textLabels.setText player2FrontTime "5.57";
  textLabels.instantShowText player1FrontTime;
  textLabels.instantShowText player2FrontTime;
  tick;

  wait 1;
  textLabels.setText player1FrontTime "5.5";
  textLabels.instantShowText player1FrontTime;
  tick;

  ; Todd Rogers epic time
  wait 1;
  textLabels.setText player1FrontTime "5.51";
  textLabels.instantShowText player1FrontTime;
  tick;
  wait 1;
  ct;
end

to shellshock
  cp;
  ask patches with [pycor <= -10 or (pycor <= -5 and pxcor >= 0)] [
    set pcolor grey;
  ]
  ask patches with [pycor = -15 and pxcor >= -15 and pxcor <= -10] [
    set pcolor green;
  ]
  ask patches with [pycor = -14 and pxcor >= -15 and pxcor <= -10] [
    set pcolor blue;
  ]
  ask patches with [(pycor = -14 or pycor = -15) and pxcor >= -9 and pxcor <= -5] [
    set pcolor red;
  ]
  let fireLabel textLabels._init_ "FIRE" true -6 -14.5;
  textLabels.setColor fireLabel black;

  let friendTank blueSquares._init_ 2;
  let enemyTank squares._init_ 2 red;
  let cursor objects._init_;

  cd
  ask cursor [
    lt 45;
    set size 1.5;
    set color white;
  ]
  ask friendTank [
    setxy -15 -9;
  ]
  ask enemyTank [
    setxy 15 -4;
  ]

  let targetX [xcor] of enemyTank - 6;
  let targetY [ycor] of enemyTank - 3;
  let slope -0.01;
  let tankTraj trajs._init_ [xcor] of friendTank [ycor] of friendTank targetX targetY slope;
  let speed 0;

  foreach (range 4) [ n ->
    let i n * 2.7
    sound:play-note-later i "cello" 45 64 0.1;
    sound:play-note-later i "trumpet" 45 64 0.1;
    sound:play-note-later i + 0.6 "cello" 45 64 0.1;
    sound:play-note-later i + 0.6 "trumpet" 45 64 0.1;
    sound:play-note-later i + 0.9 "cello" 45 64 0.1;
    sound:play-note-later i + 0.9 "trumpet" 45 64 0.1;
    sound:play-note-later i + 1.5 "cello" 45 64 0.1;
    sound:play-note-later i + 1.5 "trumpet" 45 64 0.1;
    sound:play-note-later i + 1.8 "cello" 52 64 0.1;
    sound:play-note-later i + 1.8 "trumpet" 52 64 0.1;
    sound:play-note-later i + 2.1 "cello" 52 64 0.1;
    sound:play-note-later i + 2.1 "trumpet" 52 64 0.1;
    sound:play-note-later i + 2.4 "cello" 52 64 0.1;
    sound:play-note-later i + 2.4 "trumpet" 52 64 0.1;
  ]

  ; Accelerate
  while [[xcor] of cursor > -3.5 and [ycor] of cursor > -4] [
    cd;
    objects.move cursor ((-7 / 100) * speed) ((-8 / 100) * speed)
    set speed (speed + 0.1);

    squares.draw friendTank;
    squares.draw enemyTank;
    trajs.preview tankTraj;
    tick;
  ]

  ; Deccelerate
  while [[xcor] of cursor > -7 and [ycor] of cursor > -8] [
    cd;
    objects.move cursor ((-7 / 100) * speed) ((-8 / 100) * speed)
    set speed (speed - 0.1);

    squares.draw friendTank;
    squares.draw enemyTank;
    trajs.preview tankTraj;
    tick;
  ]

  ; Adjust trajectory
  foreach (range 100) [
    cd;
    trajs.updateTraj tankTraj [xcor] of friendTank [ycor] of friendTank targetX targetY slope;
    set slope (slope - 0.0005);
    set targetX (targetX + 6 / 100);
    set targetY (targetY + 3 / 100);
    every 0.1 [
      sound:play-drum "low wood block" 64;
    ]
    objects.setPosition cursor [xcor] of cursor (item 1 (item 0 (filter [coord -> (item 0 coord) = precision [xcor] of cursor 1] [trajList] of tankTraj)) - 1)
    squares.draw friendTank;
    squares.draw enemyTank;
    trajs.preview tankTraj;
    tick;
  ]

  ; Move down to fire button
  let dist [ycor] of cursor + 15
  let halfmark [ycor] of cursor - (dist / 2)

  while [[ycor] of cursor > halfmark] [
    cd;
    objects.move cursor 0 ((-1 * dist / 100) * speed)
    set speed (speed + 0.1);

    squares.draw friendTank;
    squares.draw enemyTank;
    trajs.preview tankTraj;
    tick;
  ]

  while [[ycor] of cursor > -15] [
    cd;
    objects.move cursor 0 ((-1 * dist / 100) * speed)
    set speed (speed - 0.1);

    squares.draw friendTank;
    squares.draw enemyTank;
    trajs.preview tankTraj;
    tick;
  ]

  ; FIRE
  sound:play-drum "Acoustic Snare" 64;
  let shell blueSquares._init_ 0.5;
  ask shell [
    setxy [xcor] of friendTank [ycor] of friendTank;
  ]

  foreach (filter [n -> position n [trajList] of tankTraj mod 2 = 0] [trajList] of tankTraj) [ n ->
    cd;
    ask shell [
      setxy (item 0 n) (item 1 n);
      ; Draw trail
      hatch 1 [
        pd;
        ht;
        foreach (range (position n [trajList] of tankTraj) (max (list (position n [trajList] of tankTraj - 10) 0)) -1) [ i ->
          setxy (item 0 (item i [trajList] of tankTraj)) (item 1 (item i [trajList] of tankTraj));
        ]
        die;
      ]

    ]
    ; Drift the cursor to hide netlogo's inadequacy with text
    objects.move cursor 0.05 0.01;
    squares.draw shell;
    squares.draw friendTank;
    squares.draw enemyTank;
    tick;
  ]

  ; Explode
  cd;
  squares.draw friendTank;
  let explosion objects._init_;
  ask explosion [
    setxy [xcor] of enemyTank [ycor] of enemyTank + 0.5;
    set size 0.1;
    set shape "fire";
    set color yellow;
  ]

  sound:play-drum "Acoustic Bass Drum" 128;
  let exploSpeed 1
  while [[size] of explosion < 3.5] [
    ask explosion [
      set size (size + 0.5 * exploSpeed);
    ]
    set exploSpeed (exploSpeed + 0.5);
    tick;
  ]

  wait 0.5;

  ; Victory
  sound:play-note "cello" 48 64 0.3;
  sound:play-note "trumpet" 48 64 0.3;
  sound:play-note-later 0.4 "cello" 51 64 0.3;
  sound:play-note-later 0.4 "trumpet" 51 64 0.3;
  sound:play-note-later 0.8 "cello" 53 64 0.2;
  sound:play-note-later 0.8 "trumpet" 53 64 0.2;
  sound:play-note-later 1 "cello" 55 64 0.2;
  sound:play-note-later 1 "trumpet" 55 64 0.2;
  sound:play-note-later 1.2 "cello" 60 64 0.4;
  sound:play-note-later 1.2 "trumpet" 60 64 0.4;
  let vict1 textLabels._init_ "VICTORY!" false 2 1.5;
  let vict2 textLabels._init_ "xxBlueSquareTankGamerxx wins!" false 8 0;

  while [textLabels.hasNext vict1] [
    textLabels.showNextNLetters vict1 1;
    tick;
  ]

  wait 0.5;

  while [textLabels.hasNext vict2] [
    textLabels.showNextNLetters vict2 1;
    tick;
  ]
  wait 0.5;
  ct;
  cd;
end

to marioRun [mMario]
  ask mMario [
    if shape != "mariojump" [
      ifelse shape = "mariostand" [
        set shape "mariorun0";
      ][
        set shape (word "mariorun" ((read-from-string last shape + 1) mod 3));
      ]
    ]
    set xcor (xcor + 0.75);
  ]
end

to marioJump [mMario speed height]
  ask mMario [
    ifelse fallSpeed = 0 [
      set shape "mariojump"
      set fallSpeed speed;
      sound:play-note "Polysynth" 108 64 0.1;
    ][
      set fallSpeed (fallSpeed - 0.1);
    ]

    set ycor (max (list -8.5 (min (list height (ycor + 0.1 * fallSpeed)))));
    if ycor = height [
      set fallSpeed -1 * speed;
    ]
    if ycor = -8.5 [
      set fallSpeed 0;
      set shape "mariostand"
    ]
  ]
end

to goomsFall [goomObj]
  ask goomObj [
    set heading 180;
    set ycor (ycor + fallSpeed);
    set fallSpeed (fallSpeed - 0.1);
    if ycor <= -15.5 [
      die
    ]
  ]
end

to superMarioBros
  ask patches [
    set pcolor [173 216 230]
  ]

  let nario objects._init_;
  ask nario [
    set breed marios;
    setxy -12 -8.5
    set size 5;
    set shape "mariostand";
  ]

  foreach (range 0 33 2.75) [ i ->
    cro 1 [
      setxy (i - 16.5) -15;
      set shape "groundblock";
      set size 3.75;
    ]
    cro 1 [
      setxy (i - 16.5) -12.24;
      set shape "groundblock";
      set size 3.75;
    ]
  ]

  ; Mario runs across an empty world
  sound:play-note "french horn" 64 64 0.1;
  sound:play-note "trumpet" 64 64 0.1;
  sound:play-note-later 0.2 "french horn" 64 64 0.1;
  sound:play-note-later 0.2 "trumpet" 64 64 0.1;
  sound:play-note-later 0.6 "french horn" 64 64 0.1;
  sound:play-note-later 0.6 "trumpet" 64 64 0.1;
  sound:play-note-later 1 "french horn" 60 64 0.1;
  sound:play-note-later 1 "trumpet" 60 64 0.1;
  sound:play-note-later 1.2 "french horn" 64 64 0.1;
  sound:play-note-later 1.2 "trumpet" 64 64 0.1;
  sound:play-note-later 1.6 "french horn" 67 64 0.1;
  sound:play-note-later 1.6 "trumpet" 67 64 0.1;
  sound:play-note-later 2.2 "french horn" 55 64 0.4;
  sound:play-note-later 2.2 "trumpet" 55 64 0.4;
  while [[xcor] of nario > -14] [
    every 0.01 [
      marioRun nario;
    ]
    tick;
  ]

  ; Encounters the 1-1 blocks
  cro 1 [
    setxy -11 0;
    set shape "questionblock"
    set size 3.75;
  ]

  cro 1 [
    set shape "regblock";
    set size 3.75;
  ]

  let toHitBlock objects._init_
  ask toHitBlock [
    set shape "questionblock";
    set size 3.75;
    setxy 2.75 0;
  ]

  cro 1 [
    set shape "regblock";
    set size 3.75;
    setxy 5.5 0;
  ]

  cro 1 [
    set shape "questionblock";
    set size 3.75;
    setxy 8.25 0;
  ]

  cro 1 [
    set shape "regblock";
    set size 3.75;
    setxy 11 0;
  ]

  cro 1 [
    set shape "questionblock";
    set size 3.75;
    setxy 5.5 8.25;
  ]

  let goomba objects._init_;
  ask goomba [
    set shape "goomba";
    set size 4;
    setxy 10 -9.25;
  ]

  ; He jumps and hits a block
  let squarePowerup nobody;
  let startJump false;
  let startBlockhit false;
  let endBlockhit false;
  while [[xcor] of nario < 14] [
    every 0.01 [
      marioRun nario;
    ]

    objects.move goomba -0.5 0

    ask goomba [
      ifelse shape = "goomba" [
        set shape "goombarev";
      ][
        set shape "goomba";
      ]
    ]

    if [xcor] of nario > -3 and not startJump [
      marioJump nario 9 -2.75;
      set startJump true;
    ]

    if [ycor] of nario > -8.5 [
      marioJump nario 9 -2.75;
    ]

    if [ycor] of nario = -2.75 [
      set startBlockhit true;
    ]

    if startBlockhit [
      objects.move toHitBlock 0 0.25;
      if [ycor] of toHitBlock > 1 [
        set startBlockhit false;
        set endBlockhit true;
        set squarePowerup blueSquares._init_ 2;
        ask squarePowerup [
          setxy 2.75 [ycor] of toHitBlock;
        ]
        ask toHitBlock [
          set shape "questionhitblock";
        ]
      ]
    ]

    if endBlockhit [
      cd;
      objects.move toHitBlock 0 -0.25;

      if [ycor] of toHitBlock < 0 [
        objects.setPosition toHitBlock 2.75 0;
        set endBlockhit false;
      ]

      objects.move squarePowerup 0 0.2;
      squares.draw squarePowerup;
    ]
    tick;
  ]

  ; Now he waits to jump and grab the powerup
  ask nario [
    set shape "mariostand";
  ]

  let squareMoving true;
  let jumped false;
  let startCatch false;
  while [squareMoving] [
    if goomba != nobody [
      objects.move goomba -0.5 0

      ask goomba [
        ifelse shape = "goomba" [
          set shape "goombarev";
        ][
          set shape "goomba";
        ]
      ]

      if [xcor] of goomba <= -15 [
        objects.free goomba
      ]
    ]

    cd;
    ifelse [ycor] of squarePowerup < 2.5 [
      objects.move squarePowerup 0 0.2;
    ][
      objects.move squarePowerup 0.25 0;
      if [xcor] of squarePowerup > 12.5 and not jumped [
        marioJump nario 8 2;
        set jumped true;
      ]
    ]
    squares.draw squarePowerup;

    if [ycor] of nario > -8.5 [
      marioJump nario 9 2;
    ]

    if [pycor] of nario = 1 [
      cd;
      set squareMoving false;

      sound:play-note "piccolo" 64 64 0.1;
      sound:play-note-later 0.1 "piccolo" 67 64 0.075;
      sound:play-note-later 0.2 "piccolo" 76 64 0.075;
      sound:play-note-later 0.3 "piccolo" 72 64 0.075;
      sound:play-note-later 0.4 "piccolo" 74 64 0.075;
      sound:play-note-later 0.5 "piccolo" 79 64 0.075;
    ]

    tick;
  ]

  ; Mario obtains the powerup and runs to next screen
  squares.setSidelength squarePowerup 4;

  while [[xcor] of nario > -15] [
    cd;
    ifelse [ycor] of nario > -8.5 [
      marioJump nario 9 2;
    ][
      marioRun nario;
    ]
    objects.setPosition squarePowerup [xcor] of nario [ycor] of nario;
    objects.rot squarePowerup 5;
    squares.draw squarePowerup;
    tick;
  ]

  ask turtles with [ycor >= 0] [
    die;
  ]

  let goombaCount 0;
  let goombas (turtle-set)
  cro 8 [
    set breed gooms;
    set shape "goomba";
    set fallSpeed 1;
    set falling? false;
    set size 4;
    set heading 0
    setxy (1 + (goombaCount * 2)) -9.25;
    set goombaCount (goombaCount + 1);
    set goombas (turtle-set goombas self);
  ]
  tick;

  marioRun nario
  while [[xcor] of nario > -16] [
    cd;
    marioRun nario;
    ask goombas [
      setxy (xcor - 0.5) ycor;
      ifelse shape = "goomba" [
        set shape "goombarev";
      ][
        set shape "goomba";
      ]

      if distance nario <= 1 or falling? [
        sound:play-drum "acoustic bass drum" 96;
        goomsFall self;
        set falling? true;
      ]
    ]

    objects.setPosition squarePowerup [xcor] of nario [ycor] of nario;
    objects.rot squarePowerup 5;
    squares.draw squarePowerup;
    tick;
  ]

  ask goombas [
    die;
  ]
  ask turtles with [xcor >= -11] [
    die;
  ]

  while [[pxcor] of nario <= -9] [
    cd
    marioRun nario;

    objects.setPosition squarePowerup [xcor] of nario [ycor] of nario;
    objects.rot squarePowerup 5;
    squares.draw squarePowerup;
    tick;
  ]

  repeat 10 [
    cd;
    ask nario [
      set shape (word "mariorun" ((read-from-string last shape + 1) mod 3));
    ]
    objects.setPosition squarePowerup [xcor] of nario [ycor] of nario;
    objects.rot squarePowerup 5;
    squares.draw squarePowerup;
    tick;
  ]

  let inbetween 0;
  while [inbetween < 0.4] [
    cd;
    every inbetween [
      ask nario [
        set shape (word "mariorun" ((read-from-string last shape + 1) mod 3));
      ]
    ]

    objects.setPosition squarePowerup [xcor] of nario [ycor] of nario;
    objects.rot squarePowerup 5;
    squares.draw squarePowerup;

    set inbetween (inbetween + 0.005);
    tick;
  ]

  let pitch 120;
  while [[ycor] of nario >= -15.5] [
    cd
    sound:play-note "Guitar Fret Noise" pitch 64 0.3;
    set pitch (pitch - 10);
    objects.move nario 0 -0.75;
    objects.setPosition squarePowerup [xcor] of nario [ycor] of nario;
    objects.rot squarePowerup 5;
    if [ycor] of nario >= -14 [
      squares.draw squarePowerup;
    ]
    tick;
  ]

  objects.free nario;
  tick;
  wait 0.5;
end

to staticScreen
  ct cd cp;
  ask patches [
    set pcolor (random-float 9.9);
  ]
  sound:play-drum "Maracas" 64;
end

to errorscreen
  cp;
  let computerdesk objects._init_;
  ask computerdesk [
    set shape "computertable";
    setxy -5 0
    set size 20;
  ]

  let ivan person._init_ "" "ivan" 25;
  objects.setPosition ivan 7 0;

  let dialog dialogues._init_ "Ivan(Me)" "right" blue;
  dialogues.putText1 dialog "Alright, let's try to code Atari Dragster!";

  while [dialogues.hasNext dialog] [
    dialogues.showNextCharacter dialog
    sound:play-note "TRUMPET" 60 64 0.1;
    tick;
  ]

  wait 1;

  dialogues.free dialog;
  objects.free ivan;
  objects.free computerdesk;

  ; Ivan types the command in
  let commandcenter objects._init_;
  ask commandcenter [
    set shape "commandcenter";
    set size 33;
    set ycor -11.5;
  ]

  let consoleInput textLabels._init_ "observer> double speed = 0.0;" true 2.5 -15;
  textLabels.setColor consoleInput black;
  tick;
  wait 1;
  sound:play-drum "Hi Wood Block" 64
  let consoleOutput textLabels._init_ "ERROR: Nothing named DOUBLE has been defined." true 12 -13;
  textLabels.setColor consoleOutput black;
  tick;
  wait 2;

  ct;
  set ivan person._init_ "" "ivan" 25;
  let magnitude 2;
  ask ivan [
    set color [139 0 0]
  ]
  ask patches [
    set pcolor red;
  ]
  repeat 10 [
    ask ivan [
      set heading (random-float 360);
      fd magnitude;
    ]
    tick;
    ask ivan [
      bk magnitude;
    ]
    tick;
    set magnitude (magnitude + 0.2);
  ]

  set dialog dialogues._init_ "Ivan(Me)" "left" [102 0 0];
  dialogues.putText1 dialog "REEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE";
  dialogues.putText2 dialog "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE";

  while [dialogues.hasNext dialog] [
    sound:play-note "Acoustic Grand Piano" (random 128) 64 0.1;
    sound:play-note "Acoustic Grand Piano" (random 128) 64 0.1;
    sound:play-note "Acoustic Grand Piano" (random 128) 64 0.1;
    sound:play-note "Acoustic Grand Piano" (random 128) 64 0.1;
    sound:play-note "Acoustic Grand Piano" (random 128) 64 0.1;
    sound:play-note "Acoustic Grand Piano" (random 128) 64 0.1;
    sound:play-note "Acoustic Grand Piano" (random 128) 64 0.1;

    dialogues.showNextCharacter dialog
    ask ivan [
      ifelse pxcor = 0 and pycor = 0 [
        set heading (random-float 360);
        fd magnitude;
      ][
        bk magnitude;
        set magnitude (magnitude + 0.2);
      ]
    ]
    sound:play-note "TRUMPET" 60 64 1;
    tick;
  ]
  ct cd cp;

  let ending textLabels._init_ "The End" false 2 2
  while [textLabels.hasNext ending] [
    sound:play-drum "Mute Hi Conga" 64;
    textLabels.showNextNLetters ending 1
    tick;
  ]
  let finalSquare objects._init_;
  ask finalSquare [
    set color blue;
    set pen-size 2;
    setxy -4 4
    rt 90;
    pd;
  ]

  wait 2;
  repeat 4 [
    sound:play-drum "Hi Wood Block" 64;
    ask finalSquare [
      fd 8;
    ]
    tick;
    wait 0.5;
    sound:play-drum "Low Wood Block" 64;
    ask finalSquare [
      rt 90;
    ]
    tick;
    wait 0.5;
  ]

end

to go
  ; Setup
  ca
  reset-ticks

  intro;
  mrbrooksteaches;
  repeat 30 [
    staticscreen;
    tick;
  ]
  toddrogers;
  repeat 30 [
    staticscreen;
    tick;
  ]
  shellshock;
  repeat 30 [
    staticscreen;
    tick;
  ]
  superMarioBros;
  repeat 30 [
    staticscreen;
    tick;
  ]
  errorscreen;
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
647
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
1
1
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
64
158
127
191
NIL
go
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
29
36
179
64
Turn sound on before you start!
11
0.0
1

TEXTBOX
29
70
179
98
If sound is delayed, restart netlogo
11
0.0
1

TEXTBOX
30
110
180
138
Only tested for compatibility with 6.1.1
11
0.0
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

cloud
false
0
Circle -7500403 true true 48 63 85
Circle -7500403 true true 18 108 85
Circle -7500403 true true 93 108 85
Circle -7500403 true true 108 48 85
Circle -7500403 true true 138 63 85
Circle -7500403 true true 168 63 85
Circle -7500403 true true 198 93 85
Circle -7500403 true true 48 153 85
Circle -7500403 true true 153 123 85
Circle -7500403 true true 183 123 85
Circle -7500403 true true 153 153 85
Circle -7500403 true true 93 168 85

commandcenter
false
0
Rectangle -1 true false 0 105 315 195
Rectangle -8630108 true false 0 105 300 120
Rectangle -8630108 true false 0 175 75 195
Line -8630108 false 301 175 75 175

computertable
false
0
Rectangle -6459832 true false 0 180 330 210
Rectangle -6459832 true false 15 210 45 300
Rectangle -6459832 true false 255 210 285 315
Rectangle -7500403 true true 90 165 210 180
Rectangle -7500403 true true 135 135 165 165
Rectangle -7500403 true true 60 120 240 135
Rectangle -7500403 true true 60 15 75 120
Rectangle -7500403 true true 60 15 240 30
Rectangle -7500403 true true 225 30 240 120

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

empty
true
0

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

fire
false
0
Polygon -7500403 true true 151 286 134 282 103 282 59 248 40 210 32 157 37 108 68 146 71 109 83 72 111 27 127 55 148 11 167 41 180 112 195 57 217 91 226 126 227 203 256 156 256 201 238 263 213 278 183 281
Polygon -955883 true false 126 284 91 251 85 212 91 168 103 132 118 153 125 181 135 141 151 96 185 161 195 203 193 253 164 286
Polygon -2674135 true false 155 284 172 268 172 243 162 224 148 201 130 233 131 260 135 282

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

goomba
true
0
Rectangle -2674135 true false 105 150 195 165
Rectangle -2674135 true false 45 150 105 180
Rectangle -2674135 true false 30 120 45 165
Rectangle -2674135 true false 45 90 60 150
Rectangle -2674135 true false 60 75 75 150
Rectangle -2674135 true false 75 90 90 150
Rectangle -2674135 true false 75 60 225 75
Rectangle -2674135 true false 90 45 210 60
Rectangle -2674135 true false 105 30 195 45
Rectangle -2674135 true false 120 15 180 30
Rectangle -2674135 true false 105 75 195 90
Rectangle -2674135 true false 120 90 180 105
Rectangle -16777216 true false 75 75 105 90
Rectangle -16777216 true false 105 90 120 135
Rectangle -16777216 true false 120 105 180 120
Rectangle -16777216 true false 180 90 195 135
Rectangle -16777216 true false 195 75 225 90
Rectangle -2674135 true false 225 75 240 150
Rectangle -2674135 true false 195 150 255 180
Rectangle -2674135 true false 255 120 270 165
Rectangle -2674135 true false 240 90 255 150
Rectangle -2674135 true false 210 90 225 150
Rectangle -1 true false 90 90 105 150
Rectangle -1 true false 105 135 195 150
Rectangle -1 true false 120 120 180 135
Rectangle -1 true false 195 90 210 150
Rectangle -1 true false 90 180 210 210
Rectangle -1 true false 120 210 195 225
Rectangle -1 true false 135 225 165 255
Rectangle -1 true false 165 225 180 240
Rectangle -1 true false 105 165 195 180
Rectangle -16777216 true false 60 195 90 255
Rectangle -16777216 true false 45 210 120 240
Rectangle -16777216 true false 90 225 135 255
Rectangle -16777216 true false 165 240 210 255
Rectangle -16777216 true false 180 225 225 240
Rectangle -16777216 true false 195 210 225 225

goombarev
true
0
Rectangle -2674135 true false 105 150 195 165
Rectangle -2674135 true false 45 150 105 180
Rectangle -2674135 true false 30 120 45 165
Rectangle -2674135 true false 45 90 60 150
Rectangle -2674135 true false 60 75 75 150
Rectangle -2674135 true false 75 90 90 150
Rectangle -2674135 true false 75 60 225 75
Rectangle -2674135 true false 90 45 210 60
Rectangle -2674135 true false 105 30 195 45
Rectangle -2674135 true false 120 15 180 30
Rectangle -2674135 true false 105 75 195 90
Rectangle -2674135 true false 120 90 180 105
Rectangle -16777216 true false 75 75 105 90
Rectangle -16777216 true false 105 90 120 135
Rectangle -16777216 true false 120 105 180 120
Rectangle -16777216 true false 180 90 195 135
Rectangle -16777216 true false 195 75 225 90
Rectangle -2674135 true false 225 75 240 150
Rectangle -2674135 true false 195 150 255 180
Rectangle -2674135 true false 255 120 270 165
Rectangle -2674135 true false 240 90 255 150
Rectangle -2674135 true false 210 90 225 150
Rectangle -1 true false 90 90 105 150
Rectangle -1 true false 105 135 195 150
Rectangle -1 true false 120 120 180 135
Rectangle -1 true false 195 90 210 150
Polygon -16777216 true false 210 195 240 195 240 210 255 210 255 240 240 240 240 255 165 255 165 225 180 225 180 210 210 210
Polygon -1 true false 210 180 210 210 180 210 180 225 165 225 165 255 135 255 135 240 120 240 120 225 105 225 105 210 90 210 90 180 105 180 105 165 195 165 195 180
Polygon -16777216 true false 105 210 75 210 75 240 90 240 90 255 135 255 135 240 120 240 120 225 105 225

goombasquash
false
0
Rectangle -1 true false 30 195 255 210
Rectangle -16777216 true false 45 210 120 225
Rectangle -16777216 true false 180 210 255 225
Rectangle -1 true false 75 180 240 195
Rectangle -2674135 true false 30 165 255 180
Rectangle -16777216 true false 120 150 180 165
Rectangle -16777216 true false 75 135 120 150
Rectangle -16777216 true false 180 135 225 150
Rectangle -1 true false 180 150 240 165
Rectangle -1 true false 60 150 120 165
Rectangle -2674135 true false 30 150 60 165
Rectangle -2674135 true false 45 135 75 150
Rectangle -2674135 true false 60 120 225 135
Rectangle -2674135 true false 120 105 180 150
Rectangle -2674135 true false 225 135 255 150
Rectangle -2674135 true false 240 150 270 180

groundblock
false
0
Rectangle -16777216 true false 255 45 270 270
Rectangle -16777216 true false 30 255 270 270
Rectangle -1 true false 45 30 255 45
Rectangle -955883 true false 30 30 45 45
Rectangle -16777216 true false 165 30 180 180
Rectangle -16777216 true false 225 60 240 75
Rectangle -955883 true false 255 30 270 45
Rectangle -955883 true false 255 255 270 270
Rectangle -955883 true false 30 255 45 270
Rectangle -1 true false 150 210 165 270
Rectangle -1 true false 165 180 180 210
Rectangle -1 true false 180 120 195 180
Rectangle -1 true false 195 120 255 135
Rectangle -1 true false 180 45 195 105
Rectangle -955883 true false 180 105 195 120
Rectangle -16777216 true false 150 180 165 210
Rectangle -16777216 true false 30 180 60 195
Rectangle -16777216 true false 60 195 90 210
Polygon -16777216 true false 90 210 90 210 150 210 150 255 135 255 135 225 90 225 90 210
Polygon -16777216 true false 195 90 210 90 210 105 255 105 255 120 195 120 195 90
Polygon -955883 true false 195 45 195 90 210 90 210 105 255 105 255 45
Polygon -1 true false 30 255 30 195 60 195 60 210 45 210 45 255
Rectangle -1 true false 60 210 90 225
Rectangle -1 true false 90 225 135 240
Rectangle -955883 true false 135 255 150 270
Rectangle -955883 true false 180 30 195 45
Rectangle -1 true false 30 45 45 180
Rectangle -955883 true false 255 105 270 120
Polygon -955883 true false 45 45 45 180 60 180 60 195 90 195 90 210 150 210 150 180 165 180 165 45
Polygon -955883 true false 45 210 60 210 60 225 90 225 90 240 135 240 135 255 45 255
Polygon -955883 true false 195 135 255 135 255 255 165 255 165 210 180 210 180 180 195 180

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

ivan
false
0
Circle -2064490 true false 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105
Polygon -13345367 true false 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195
Rectangle -13345367 true false 150 90 150 90
Line -16777216 false 150 90 150 195
Line -2064490 false 120 90 120 90
Line -16777216 false 128 90 172 90
Line -16777216 false 120 30 120 45
Rectangle -16777216 false false 120 30 135 45
Rectangle -16777216 false false 150 30 165 45
Line -16777216 false 134 38 150 38

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

mariojump
false
0
Rectangle -6459832 true false 165 90 180 120
Rectangle -6459832 true false 180 120 195 135
Rectangle -6459832 true false 165 135 225 150
Rectangle -2064490 true false 90 105 105 135
Rectangle -2674135 true false 165 180 165 225
Rectangle -6459832 true false 225 165 225 165
Rectangle -6459832 true false 210 135 225 165
Rectangle -6459832 true false 210 135 240 150
Rectangle -6459832 true false 210 90 255 120
Rectangle -6459832 true false 240 120 255 135
Rectangle -2674135 true false 120 165 135 180
Rectangle -2674135 true false 180 165 195 180
Rectangle -2674135 true false 135 180 150 270
Rectangle -2674135 true false 150 195 210 210
Rectangle -2674135 true false 195 180 210 210
Rectangle -1184463 true false 150 210 165 225
Rectangle -1184463 true false 195 210 210 225
Rectangle -2674135 true false 210 210 225 255
Rectangle -2674135 true false 165 210 195 255
Rectangle -2674135 true false 150 225 180 270
Rectangle -2674135 true false 195 225 225 255
Rectangle -2674135 true false 120 210 135 285
Rectangle -2674135 true false 105 225 120 285
Rectangle -2674135 true false 90 240 105 285
Rectangle -2674135 true false 75 210 105 240
Rectangle -2674135 true false 75 255 90 285
Rectangle -6459832 true false 30 255 45 285
Rectangle -6459832 true false 45 240 75 270
Rectangle -6459832 true false 60 225 75 255
Rectangle -6459832 true false 75 240 90 255
Rectangle -2064490 true false 30 195 45 240
Rectangle -2064490 true false 15 195 45 225
Rectangle -2064490 true false 45 210 60 225
Rectangle -6459832 true false 135 165 180 180
Rectangle -6459832 true false 150 180 195 195
Rectangle -6459832 true false 195 165 210 180
Rectangle -6459832 true false 225 210 255 255
Rectangle -6459832 true false 240 180 255 210
Polygon -2064490 true false 105 135 105 165 210 165 210 150 165 150 165 135 180 135 180 120 165 120 165 90 135 90 135 105 120 105 120 120 135 120 135 135
Polygon -6459832 true false 90 90 135 90 135 105 120 105 120 120 135 120 135 135 105 135 105 105 90 105
Polygon -6459832 true false 90 105 75 105 75 150 105 150 105 135 90 135
Polygon -2674135 true false 90 90 90 75 105 75 105 60 165 60 165 75 225 75 225 90
Polygon -2064490 true false 210 75 210 45 255 45 255 90 225 90 225 75
Polygon -2064490 true false 180 90 195 90 195 105 210 105 210 120 240 120 240 135 195 135 195 120 180 120
Polygon -6459832 true false 120 165 45 165 45 180 30 180 30 195 45 195 45 210 60 210 105 210 105 225 120 225 120 210 135 210 135 180 120 180

mariorun0
false
0
Rectangle -2674135 true false 105 45 180 60
Rectangle -2674135 true false 90 60 225 75
Rectangle -6459832 true false 90 75 135 90
Rectangle -2064490 true false 135 75 165 90
Rectangle -6459832 true false 165 75 180 105
Rectangle -2064490 true false 120 90 165 105
Rectangle -2064490 true false 180 75 195 105
Rectangle -2064490 true false 195 90 225 120
Rectangle -2064490 true false 225 105 240 120
Rectangle -6459832 true false 180 105 195 120
Rectangle -6459832 true false 165 120 225 135
Rectangle -2064490 true false 135 105 180 120
Rectangle -6459832 true false 105 90 120 120
Rectangle -6459832 true false 120 105 135 120
Rectangle -6459832 true false 75 90 90 135
Rectangle -6459832 true false 90 120 105 135
Rectangle -2064490 true false 90 90 105 120
Rectangle -2064490 true false 105 120 165 150
Rectangle -2064490 true false 165 135 210 150
Rectangle -2674135 true false 105 195 120 240
Rectangle -2674135 true false 120 225 165 240
Rectangle -2674135 true false 60 225 75 240
Rectangle -2674135 true false 90 195 105 255
Rectangle -2674135 true false 75 210 90 255
Rectangle -2674135 true false 120 210 135 240
Rectangle -2674135 true false 165 210 195 240
Rectangle -2674135 true false 180 210 210 255
Rectangle -2674135 true false 180 195 195 210
Rectangle -2064490 true false 60 180 75 195
Rectangle -2064490 true false 210 165 255 180
Rectangle -2064490 true false 225 165 255 195
Rectangle -6459832 true false 195 165 210 180
Rectangle -6459832 true false 135 150 180 180
Rectangle -6459832 true false 180 165 195 180
Rectangle -6459832 true false 195 180 210 195
Rectangle -2674135 true false 165 240 180 255
Rectangle -6459832 true false 210 210 240 255
Rectangle -6459832 true false 225 195 240 210
Rectangle -1184463 true false 135 180 150 195
Rectangle -6459832 true false 60 270 105 285
Rectangle -6459832 true false 60 255 90 270
Rectangle -6459832 true false 45 240 75 270
Rectangle -2674135 true false 105 240 120 255
Rectangle -2674135 true false 135 210 150 225
Rectangle -6459832 true false 90 150 120 195
Rectangle -6459832 true false 60 150 90 180
Rectangle -2064490 true false 30 165 60 210
Rectangle -2674135 true false 120 150 135 210
Rectangle -2674135 true false 135 195 180 210
Rectangle -2674135 true false 165 180 165 225
Rectangle -2674135 true false 150 180 165 225
Rectangle -2674135 true false 135 150 150 180
Rectangle -2674135 true false 150 165 165 180
Rectangle -2674135 true false 165 180 195 195
Rectangle -6459832 true false 210 180 225 195

mariorun1
false
0
Rectangle -2674135 true false 120 180 150 195
Rectangle -2674135 true false 105 45 180 60
Rectangle -2674135 true false 90 60 225 75
Rectangle -6459832 true false 90 75 135 90
Rectangle -2064490 true false 135 75 165 90
Rectangle -6459832 true false 165 75 180 105
Rectangle -2064490 true false 120 90 165 105
Rectangle -2064490 true false 180 75 195 105
Rectangle -2064490 true false 195 90 225 120
Rectangle -2064490 true false 225 105 240 120
Rectangle -6459832 true false 180 105 195 120
Rectangle -6459832 true false 165 120 225 135
Rectangle -2064490 true false 135 105 180 120
Rectangle -6459832 true false 105 90 120 120
Rectangle -6459832 true false 120 105 135 120
Rectangle -6459832 true false 75 90 90 135
Rectangle -6459832 true false 90 120 105 135
Rectangle -2064490 true false 90 90 105 120
Rectangle -2064490 true false 105 120 165 150
Rectangle -2064490 true false 165 135 210 150
Rectangle -2674135 true false 165 180 165 225
Rectangle -2674135 true false 120 150 135 165
Rectangle -2674135 true false 135 165 165 180
Rectangle -2674135 true false 135 195 210 210
Rectangle -2674135 true false 165 180 195 240
Rectangle -2674135 true false 150 225 180 240
Rectangle -2674135 true false 105 240 150 255
Rectangle -2674135 true false 90 225 105 240
Rectangle -2674135 true false 75 210 90 225
Rectangle -2674135 true false 180 195 210 225
Rectangle -1184463 true false 150 180 165 195
Rectangle -1184463 true false 195 180 210 195
Rectangle -2064490 true false 120 210 150 240
Rectangle -2064490 true false 150 210 165 225
Rectangle -6459832 true false 105 195 120 240
Rectangle -6459832 true false 90 195 105 225
Rectangle -6459832 true false 120 195 135 210
Rectangle -6459832 true false 75 165 90 210
Rectangle -6459832 true false 90 150 120 195
Rectangle -6459832 true false 120 165 135 180
Rectangle -6459832 true false 105 255 165 285
Rectangle -6459832 true false 150 240 195 270
Rectangle -6459832 true false 195 255 210 270
Rectangle -6459832 true false 135 150 180 165
Rectangle -6459832 true false 165 165 195 180

mariorun2
false
0
Rectangle -2674135 true false 105 60 180 75
Rectangle -2674135 true false 90 75 225 90
Rectangle -6459832 true false 90 90 135 105
Rectangle -2064490 true false 135 90 165 105
Rectangle -6459832 true false 165 90 180 120
Rectangle -2064490 true false 120 105 165 120
Rectangle -2064490 true false 180 90 195 120
Rectangle -2064490 true false 195 105 225 135
Rectangle -2064490 true false 225 120 240 135
Rectangle -6459832 true false 180 120 195 135
Rectangle -6459832 true false 165 135 225 150
Rectangle -2064490 true false 135 120 180 135
Rectangle -6459832 true false 105 105 120 135
Rectangle -6459832 true false 120 120 135 135
Rectangle -6459832 true false 75 105 90 150
Rectangle -6459832 true false 90 135 105 150
Rectangle -2064490 true false 90 105 105 135
Rectangle -2064490 true false 105 135 165 165
Rectangle -2064490 true false 165 150 210 165
Rectangle -2674135 true false 165 180 165 225
Rectangle -2064490 true false 195 165 210 210
Rectangle -2064490 true false 180 180 225 195
Rectangle -2064490 true false 180 195 195 210
Rectangle -2674135 true false 150 165 165 180
Rectangle -6459832 true false 90 165 150 195
Rectangle -6459832 true false 105 180 180 210
Rectangle -6459832 true false 165 165 180 195
Rectangle -2064490 true false 75 180 90 210
Rectangle -2064490 true false 60 195 75 210
Rectangle -2674135 true false 90 195 105 255
Rectangle -6459832 true false 60 210 90 225
Rectangle -6459832 true false 60 225 75 255
Rectangle -6459832 true false 45 240 60 270
Rectangle -2674135 true false 75 225 120 255
Rectangle -2674135 true false 105 210 195 240
Rectangle -2674135 true false 135 240 180 255
Rectangle -6459832 true false 120 255 165 285
Rectangle -6459832 true false 165 270 180 285

mariostand
false
0
Rectangle -2674135 true false 105 45 180 60
Rectangle -2674135 true false 90 60 225 75
Rectangle -6459832 true false 90 75 135 90
Rectangle -2064490 true false 135 75 165 90
Rectangle -6459832 true false 165 75 180 105
Rectangle -2064490 true false 120 90 165 105
Rectangle -2064490 true false 180 75 195 105
Rectangle -2064490 true false 195 90 225 120
Rectangle -2064490 true false 225 105 240 120
Rectangle -6459832 true false 180 105 195 120
Rectangle -6459832 true false 165 120 225 135
Rectangle -2064490 true false 135 105 180 120
Rectangle -6459832 true false 105 90 120 120
Rectangle -6459832 true false 120 105 135 120
Rectangle -6459832 true false 75 90 90 135
Rectangle -6459832 true false 90 120 105 135
Rectangle -2064490 true false 90 90 105 120
Rectangle -2064490 true false 105 120 165 150
Rectangle -2064490 true false 165 135 210 150
Rectangle -2674135 true false 120 210 135 255
Rectangle -2674135 true false 120 210 165 225
Rectangle -2674135 true false 105 180 120 240
Rectangle -2674135 true false 90 210 105 255
Rectangle -2674135 true false 120 210 135 240
Rectangle -2674135 true false 165 195 195 225
Rectangle -2674135 true false 165 225 210 255
Rectangle -2674135 true false 180 195 195 210
Rectangle -2064490 true false 90 210 105 225
Rectangle -2064490 true false 195 210 210 225
Rectangle -2064490 true false 210 195 240 240
Rectangle -6459832 true false 165 150 180 165
Rectangle -6459832 true false 180 255 225 285
Rectangle -6459832 true false 135 150 165 180
Rectangle -6459832 true false 195 180 210 210
Rectangle -2674135 true false 165 225 180 240
Rectangle -6459832 true false 180 165 225 180
Rectangle -6459832 true false 225 270 240 285
Rectangle -6459832 true false 60 270 90 285
Rectangle -6459832 true false 75 255 120 285
Rectangle -2674135 true false 105 240 120 255
Rectangle -2674135 true false 135 210 150 225
Rectangle -6459832 true false 75 165 105 210
Rectangle -6459832 true false 60 180 105 195
Rectangle -2064490 true false 60 195 90 240
Rectangle -2674135 true false 120 150 135 210
Rectangle -2674135 true false 135 195 180 210
Rectangle -2674135 true false 165 180 165 225
Rectangle -2674135 true false 135 180 165 240
Rectangle -2674135 true false 165 165 180 180
Rectangle -2674135 true false 135 180 165 195
Rectangle -2674135 true false 165 180 195 195
Rectangle -6459832 true false 210 180 240 195
Rectangle -1184463 true false 120 180 135 195
Rectangle -6459832 true false 90 150 120 180
Rectangle -1184463 true false 165 180 180 195

mrbrooks
false
0
Polygon -2064490 true false 219 152 219 182 249 181 249 152 219 152
Rectangle -2064490 true false 50 150 72 172
Circle -2064490 true false 110 5 80
Polygon -1 true false 105 90 105 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 195 195 195 90
Rectangle -1 true false 127 79 172 94
Polygon -1 true false 195 90 255 150 225 195 165 105
Polygon -1 true false 105 90 45 150 75 180 135 105
Polygon -16777216 true false 105 195 75 285 105 300 135 300 150 225 165 300 195 300 225 285 195 195 120 195
Rectangle -16777216 true false 120 90 120 90
Rectangle -16777216 true false 105 90 120 195
Rectangle -16777216 true false 180 90 195 195
Polygon -2674135 true false 135 90 150 105 135 135 150 180 165 135 150 105 165 90 135 90
Rectangle -2064490 true false 45 165 45 180
Rectangle -16777216 true false 105 195 105 195
Rectangle -16777216 true false 105 180 120 180
Polygon -6459832 true false 105 195 99 210 200 210 195 194
Rectangle -1184463 true false 143 194 158 210
Polygon -7500403 true true 120 15 105 30 105 60 120 45 120 15
Polygon -7500403 true true 180 15 195 30 195 60 180 45 180 15
Polygon -7500403 true true 120 60 150 75 180 60 180 75 150 90 120 75 120 60
Polygon -6459832 true false 75 270 60 285 60 300 135 300 142 270 75 270
Polygon -6459832 true false 225 270 240 285 240 300 165 300 158 270 225 270

mrbrookspointing
false
0
Polygon -2064490 true false 219 152 219 182 249 181 249 152 219 152
Rectangle -2064490 true false 35 75 57 97
Circle -2064490 true false 110 5 80
Polygon -1 true false 105 90 105 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 195 195 195 90
Rectangle -1 true false 127 79 172 94
Polygon -1 true false 195 90 255 150 225 195 165 105
Polygon -1 true false 105 90 60 60 30 105 150 165
Polygon -16777216 true false 105 195 75 285 105 300 135 300 150 225 165 300 195 300 225 285 195 195 120 195
Rectangle -16777216 true false 120 90 120 90
Rectangle -16777216 true false 105 90 120 195
Rectangle -16777216 true false 180 90 195 195
Polygon -2674135 true false 135 90 150 105 135 135 150 180 165 135 150 105 165 90 135 90
Rectangle -2064490 true false 45 165 45 180
Rectangle -16777216 true false 105 195 105 195
Rectangle -16777216 true false 105 180 120 180
Polygon -6459832 true false 105 195 99 210 200 210 195 194
Rectangle -1184463 true false 143 194 158 210
Polygon -7500403 true true 120 15 105 30 105 60 120 45 120 15
Polygon -7500403 true true 180 15 195 30 195 60 180 45 180 15
Polygon -7500403 true true 120 60 150 75 180 60 180 75 150 90 120 75 120 60
Polygon -6459832 true false 75 270 60 285 60 300 135 300 142 270 75 270
Polygon -6459832 true false 225 270 240 285 240 300 165 300 158 270 225 270

mrbrookstpose
false
0
Rectangle -2064490 true false 260 90 282 112
Rectangle -2064490 true false 20 90 42 112
Circle -2064490 true false 110 5 80
Polygon -1 true false 105 90 105 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 195 195 195 90
Rectangle -1 true false 127 79 172 94
Polygon -1 true false 195 90 270 90 270 120 165 135
Polygon -1 true false 105 90 30 90 30 120 135 135
Polygon -16777216 true false 105 195 75 285 105 300 135 300 150 225 165 300 195 300 225 285 195 195 120 195
Rectangle -16777216 true false 120 90 120 90
Rectangle -16777216 true false 105 90 120 195
Rectangle -16777216 true false 180 90 195 195
Polygon -2674135 true false 135 90 150 105 135 135 150 180 165 135 150 105 165 90 135 90
Rectangle -2064490 true false 45 165 45 180
Rectangle -16777216 true false 105 195 105 195
Rectangle -16777216 true false 105 180 120 180
Polygon -6459832 true false 105 195 99 210 200 210 195 194
Rectangle -1184463 true false 143 194 158 210
Polygon -7500403 true true 120 15 105 30 105 60 120 45 120 15
Polygon -7500403 true true 180 15 195 30 195 60 180 45 180 15
Polygon -7500403 true true 120 60 150 75 180 60 180 75 150 90 120 75 120 60
Polygon -6459832 true false 75 270 60 285 60 300 135 300 142 270 75 270
Polygon -6459832 true false 225 270 240 285 240 300 165 300 158 270 225 270

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

person police
false
0
Polygon -1 true false 124 91 150 165 178 91
Polygon -13345367 true false 134 91 149 106 134 181 149 196 164 181 149 106 164 91
Polygon -13345367 true false 180 195 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285
Polygon -13345367 true false 120 90 105 90 60 195 90 210 116 158 120 195 180 195 184 158 210 210 240 195 195 90 180 90 165 105 150 165 135 105 120 90
Rectangle -7500403 true true 123 76 176 92
Circle -7500403 true true 110 5 80
Polygon -13345367 true false 150 26 110 41 97 29 137 -1 158 6 185 0 201 6 196 23 204 34 180 33
Line -13345367 false 121 90 194 90
Line -16777216 false 148 143 150 196
Rectangle -16777216 true false 116 186 182 198
Rectangle -16777216 true false 109 183 124 227
Rectangle -16777216 true false 176 183 195 205
Circle -1 true false 152 143 9
Circle -1 true false 152 166 9
Polygon -1184463 true false 172 112 191 112 185 133 179 133
Polygon -1184463 true false 175 6 194 6 189 21 180 21
Line -1184463 false 149 24 197 24
Rectangle -16777216 true false 101 177 122 187
Rectangle -16777216 true false 179 164 183 186

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

questionblock
false
0
Rectangle -1184463 true false 45 45 255 255
Rectangle -16777216 true false 255 30 270 270
Rectangle -16777216 true false 30 255 270 270
Rectangle -955883 true false 30 45 45 255
Rectangle -955883 true false 45 30 255 45
Rectangle -16777216 true false 30 30 45 45
Rectangle -16777216 true false 60 60 75 75
Rectangle -16777216 true false 225 60 240 75
Rectangle -16777216 true false 225 225 240 240
Rectangle -16777216 true false 60 225 75 240
Rectangle -955883 true false 90 90 120 135
Rectangle -955883 true false 105 75 180 90
Rectangle -955883 true false 165 90 195 150
Rectangle -955883 true false 150 135 165 180
Rectangle -955883 true false 135 150 150 180
Rectangle -955883 true false 135 195 165 225
Rectangle -16777216 true false 105 135 135 150
Rectangle -16777216 true false 120 90 135 150
Rectangle -16777216 true false 135 90 165 105
Rectangle -16777216 true false 195 105 210 165
Rectangle -16777216 true false 165 150 210 165
Rectangle -16777216 true false 165 150 180 195
Rectangle -16777216 true false 150 180 165 195
Rectangle -16777216 true false 150 225 180 240
Rectangle -16777216 true false 165 210 180 225

questionhitblock
false
0
Rectangle -955883 true false 45 45 255 255
Rectangle -16777216 true false 255 30 270 270
Rectangle -16777216 true false 30 255 270 270
Rectangle -16777216 true false 30 45 45 255
Rectangle -16777216 true false 45 30 255 45
Rectangle -16777216 true false 30 30 45 45
Rectangle -16777216 true false 60 60 75 75
Rectangle -16777216 true false 225 60 240 75
Rectangle -16777216 true false 225 225 240 240
Rectangle -16777216 true false 60 225 75 240

regblock
false
0
Rectangle -955883 true false 30 30 135 75
Rectangle -16777216 true false 75 90 90 135
Rectangle -955883 true false 150 30 255 75
Rectangle -955883 true false 90 90 195 135
Rectangle -955883 true false 30 150 135 195
Rectangle -955883 true false 150 150 255 195
Rectangle -955883 true false 90 210 195 255
Rectangle -1 true false 30 15 270 30
Rectangle -955883 true false 30 90 75 135
Rectangle -955883 true false 210 90 270 135
Rectangle -955883 true false 30 210 75 255
Rectangle -955883 true false 210 210 270 255
Rectangle -16777216 true false 30 75 270 90
Rectangle -16777216 true false 30 135 270 150
Rectangle -16777216 true false 30 195 270 210
Rectangle -16777216 true false 195 90 210 135
Rectangle -16777216 true false 135 30 150 75
Rectangle -16777216 true false 135 150 150 195
Rectangle -16777216 true false 75 210 90 255
Rectangle -16777216 true false 195 210 210 255
Rectangle -16777216 true false 30 255 270 270
Rectangle -16777216 true false 255 30 270 75
Rectangle -16777216 true false 255 150 270 195

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

textbox
false
0
Circle -7500403 true true 0 105 30
Circle -7500403 true true 0 165 30
Circle -7500403 true true 270 105 30
Circle -7500403 true true 270 165 30
Rectangle -7500403 true true 15 105 285 195
Rectangle -7500403 true true 0 120 300 180

toddrogersback
false
0
Rectangle -13345367 true false 0 135 300 150
Rectangle -13345367 true false 90 120 120 135
Rectangle -13345367 true false 45 120 75 135
Rectangle -13345367 true false 180 120 210 135
Rectangle -13345367 true false 135 120 165 135
Rectangle -13345367 true false 225 120 255 135
Rectangle -13345367 true false 0 120 30 135
Rectangle -13345367 true false 270 120 300 135

toddrogersfront
false
0
Rectangle -16777216 true false 0 150 300 165

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

tvscreen
false
0
Rectangle -7500403 true true 0 60 15 225
Rectangle -7500403 true true 15 60 300 75
Rectangle -7500403 true true 15 210 300 225
Rectangle -7500403 true true 285 60 300 225

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
