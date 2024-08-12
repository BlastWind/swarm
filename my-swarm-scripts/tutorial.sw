/*************************************************************/
/*                      Generics                             */
/*************************************************************/

def m1 = move; end
def m4 = move; move; move; move end
def m5 = m4; move end
def m3 = move; move; move; end

def m2 = move; move; end

def m16 = m4; m4; m4; m4 end

def x3 = \c. c; c; c; end
def x4 = \c. c; c; c; c end


/*************************************************************/
/*                  Tutorial: Lambda                         */
/*************************************************************/
def five = turn North; m2; turn West; m4; turn North; m2; turn East; m4 end

def five' = turn West; m4;  turn South; m2; turn East; m4; turn South; m2; end 

def go = turn East; m4; x4 five; m2; turn South; m16; turn East; m2; end
def go' = turn West; m2; turn North; m16; turn West; m2; x4 five'; turn West; m4; end 

def toFlower = x4 go end
def toBase = x4 go' end

def solveLambda = build { toFlower; flower <- grab; toBase; give base flower } end




/*************************************************************/
/*                  Tutorial: Require devices                */
/*************************************************************/
def solveRequireDevices = build { log ""; require "boat"; turn East; m5; flw <- grab; turn West; m5; give base flw } end


/*************************************************************/
/*                  Tutorial: Require inventory              */
/*************************************************************/
def solveRequireDevices = build { log ""; require "boat"; turn East; m5; flw <- grab; turn West; m5; give base flw } end

// place rock, move
def prm = place "rock"; move; end
def pr = place "rock"; end
def prRow = prm; prm; prm; pr; end
// place rock in the 4 x 4 square (precondition: at top-left corner, have enough rocks)
def pr4by4 = turn South; prRow; turn East; move; turn North; prRow; turn East; move; turn South; prRow; turn East; move; turn North; prRow; end
def solveRequireInventory = build { require 16 "rock"; turn East; move; turn South; move; pr4by4; } end


/*************************************************************/
/*                  Tutorial: Conditionals                   */
/*************************************************************/
// here rock? Pick up if so. Move.
def vsr = "very small rock" end
def hr = b <- ishere vsr; if b { log "rock detected!"; grab; return () } { }; end 

// Execute f before visiting each square in 4x4 grid. Precondition: Start at top-left corner.
def f4by4 = \f. turn South; x3 (f; m1); f; turn East; move; turn North; x3 (f; m1); f; turn East; move; turn South; x3 (f; m1); f; turn East; move; turn North; x3 (f; m1); f; end

def solveConditionals = build { log "starting"; turn South; move; turn East; move; f4by4 hr; move; turn West; m4; x4 (give base vsr)} end
