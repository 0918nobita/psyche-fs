module Psyche.Compiler

open System.IO

let compile () =
    use writer = new BinaryWriter(File.Open("out.psyb", FileMode.Create))
    writer.Write("PSYCHE"B) // MAGIC
    writer.Write(1u) // VERSION
