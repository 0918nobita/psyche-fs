module Psyche.Compiler

open System.IO

let compile () =
    use writer = new BinaryWriter(File.Open("out.psyb", FileMode.Create))
    writer.Write("PSYC"B) // MAGIC
    writer.Write(1u)      // VERSION
    writer.Write(1uy)     // ldi
    writer.Write(3)       //   3
    writer.Write(1uy)     // ldi
    writer.Write(4)       //   4
    writer.Write(10uy)    // add
