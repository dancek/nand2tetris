// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

    // clear R2
    @R2
    M=0
    // if R0=0 end (keep this out of the loop for perf ":D")
    @R0
    D=M
    @END
    D;JLE
(LOOP)
    @R1
    D=M
    @R2
    M=D+M
    @R0
    MD=M-1
    @LOOP
    D;JGT
(END)

// TODO: is adapting Duff's device possible here?