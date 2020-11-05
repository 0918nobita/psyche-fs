namespace Psyche.Base

/// <summary>Polymorphic lens represented as a getter/setter pair</summary>
/// <typeparam name="S">the source of a PLens</typeparam>
/// <typeparam name="T">the modified source of a PLens</typeparam>
/// <typeparam name="A">the target of a PLens</typeparam>
/// <typeparam name="B">the modified target of a PLens</typeparam>
type IPLens<'S, 'T, 'A, 'B> =
    abstract member Get: 'S -> 'A
    abstract member Set: 'B -> 'T
    abstract member Modify: ('A -> 'B) -> 'S -> 'T

type ILens<'S, 'A> = IPLens<'S, 'S, 'A, 'A>
