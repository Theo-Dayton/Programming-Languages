
**F**.
After completing the rest of the assignment, please download [the text
version of the followup questions below](small-followup.md) and fill
in your answers.  Many of the questions ask about particular coding
scenarios; you are welcome to answer these questions by experimenting
with code, by thinking through the scenarios without running any code,
or by any combination thereof.

 1. While implementing large signed integers, you implement `+` and
    `*`, and you use methods for `div:` and `mod:` that are provided
    for you.  Why isn't subtraction defined on the large-integer
    classes?  When large integers are subtracted, what code you wrote,
    if any, is run?

    > Because substraction is defined by calling addLargeNegativeIntegerTo, 
      which is part of the `+` function
    > When large numbers are substracted, my code calls
      addLargeNegativeIntegerTo instead of addLargePositiveIntegerTo


 1. C++ has what is called "subclass polymorphism": if a function is
    has a formal parameter of class $C$, a caller can pass any instance
    of class $C$ or any instance of any class $C'$ that _inherits_
    from $C$.
    Class $C$' is called a _subclass_ of $C$.

    Smalltalk and Ruby have what is called "subtype polymorphism": if
    a message is expecting an argument that understands protocol $P$,
    a caller can pass any object with protocol $P'$, provide that
    $P'$ understands all the messages in $P$ (and in both protocols,
    the messages must have with the same meaning).
    Protocol $P'$ is called a _subtype_ of $P$.

    Answer this question:

     a. After you have modified the predefined classes to complete
        [Exercise ${ref/small.ex.mixed-arith-a}](#frac_and_int), is
        the `Integer` protocol a subtype of the `Fraction` protocol?
        Based on your implementation, justify your answer.

        (If you are submitting as a pair, say _which_ individual
        implementation forms the basis for your answer.)

        > Yes, because when an Integer would usually not be able to answer 
        num () or den () like a Fraction, one can always use coercion. This
        works both ways

 2. This question illustrates some possibilities that arise when
    contracts are written without reference to representation.
    Depending on which representation of natural numbers you
    implemented, answer **one** of the following two parts:
    
     a. If you chose the array representation: When message `divBase`
        is sent to a natural number $X$, it answers $X \mathbin{\mathrm{div}}
        b$, where
        $b$ is the base of natural numbers.  In the subclass
        representation, `divBase` usually just answers an instance
        variable.  Explain how you would implement `divBase`
        _efficiently_ using the array representation.  You may write
        an explanation in informal English, or you may just write the
        code.

        > 
        > 
        > 
        > 
        > 
        > 
        > 
        > 
        > 
        
     
     b. If you chose the subclass representation: A natural number $X$
        can be viewed a sum of digits $x_i$ times powers of $b$, as in
        $$ X = \sum_{i=0}^n x_i \cdot b^i\text{, where } 0 \le x_i <
        b\text{.}$$
        When message `digit:` $i$ is sent to a natural number,
        it answers $x_i$.
        In the array representation, `digit:` is usually implemented
        by an array lookup.  To explain how you would implement `digit:`
        _efficiently_ using the subclass representation, complete the
        following two method definitions.**Write no English.**
        
        >
        > ```
        > (NatZero addSelector:withMethod: digit:
        >    (compiled-method (i) ...))
        >
        > (NatNonzero addSelector:withMethod: digit:
        >    (compiled-method (i) ...))
        > ```

        
 3. This question is about the internals of natural numbers.
    Depending on which representation of natural numbers you
    implemented, answer **one** of the following two parts:
    
    a.  If you chose the array representation: Adding private methods
        `digit:` and `digit:put:` complicates the private protocol for
        class `Natural`.  A simpler protocol might be better.  And after
        the fact, it seems like it would be easy enough to remove these methods
        by
        inlining their code.  Maybe they should never have been there.

        Answer this question: if `digit:` and `digit:put:` had not
        been suggested, how would your development experience have
        been different?  Do these methods provide enough benefit to
        justify complicating the protocol?

        > 
        > 
        > 
        > 
        > 
        > 
        > 
        > 
        > 
        > 

    b.  If you chose the subclass representation: Suppose class method
        `first:rest:` *never* creates an instance of `NatZero`.
        Suppose instead that an instance of `NatZero` is created
        _only_ by class method `fromSmall:` when it receives 0.  Under
        this supposition, natural-number arithmetic may allocate more
        objects and send more messages than the code we recommend.
        But will it still work?  If not, what goes wrong?  Justify
        your answer.

        > Does not work because first:rest: ensures that the key invariants
        are mantained, if this is done through fromSmall:, they are not
        maintained and can cause errrors

 4. Suppose the multiplication method on small integers is changed so
    that it always promotes `self` to a large integer:

    ```
    (method * (anInteger) ((self asLargeInteger) * anInteger))
    ```

    This code seems inefficient, but it has the virtue of being
    simple.  Will it work?  If not, explain what goes wrong.

    > If two small integers are added this way, then the algorithm should
    be to use a hardware addition instruction, but if the small integer
    self promotes `self` to a large integer, then this is not possible
    

 5. If you're trying to design or debug code in a procedural or
    functional language, you can usually make progress by figuring out
    what function is being called and inspecting its source code.
    But with dynamic dispatch, this strategy doesn't work: a single
    call site often invokes many different methods, and tracing all
    the paths through the code is almost impossible.  (This is why
    languages like Java and JavaScript require such sophisticated
    compilers.)

    a.  How well did "trust the contract" work for you as a strategy
        for design and debugging?

        > Trusting the contract not always works because trusting the contract
          means that looking only at the contract will allow to debug and test.
          But when using dynamic and double dispatch, this kind of testing
          will sometimes fail.

    b.  Name a message you had to debug where you wanted to see what
        code would run when the message was sent, but it was
        difficult.
        (If there was no such message, name a message where you wanted
        to see code but _didn't_ need to debug.  If you never wanted
        to see code, then explain how you handled
        `multiplyBySmallInteger:` without wanting to see code.)

        > For example, debugging first:rest: was not easy because it was a
         method in both Natural and NatNonZero, so it was not obvious which
         code was running

    c.  Was the message associated with a contract?  If so, what
        was the contract?  Was the contract testable?

        > The contract was "answers a natural number representing aninteger
        + anatural * b". The contract was testable

    d.  In a functional language, ideal unit tests exercise all
        combinations of forms of input data.  In an object-oriented
        language, what analogous thing should unit tests ideally
        exercise?
        
        > In OOP, unit tests should exercise all types of classes and
         subclasses
