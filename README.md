PASTA
=====

A smart assembler for 65(C)02 processors.

Version
-------

The current version is 0.11.

Why another 6502 assembler?
---------------------------

Pasta is a cross-assembler for the MOS 6502 (also including 65C02
instructions), which contains experimental support for automatic
management of certain resources (namely, zero-page memory locations),
provided that you are prepared to use a slightly restricted programming
style.

The problem we're trying to solve is as follows. Say you write a routine in
assembler, (say) to multiply a 16-bit number in the A and X registers (with A as
the least-significant byte) by three. Hoping for small and fast code, you pick
some zero-page locations to use for temporary storage, say $70 and $71. Now your
routine might look like this (untested!):

	multiply_xa_by_3:
		; original number in [x:a].
		sta $70
		stx $71
		asl a
		rol $71
		; now [$71:a] are the original number * 2.
		clc
		adc $70
		sta $70
		txa
		adc $71
		tax
		lda $70
		; now [x:a] should be the original number * 3.
		rts

Now, you want to write another routine which calls this function. Remembering
that you've used $70 and $71 already in this function, you must either save
those locations around call sites (big and slow!), or choose new zero-page
locations. So we might write something like this:

	multiply_xa_by_5:
		sta $72
		stx $73
		jsr multiply_xa_by_3
		sta $74
		stx $75

		; multiply [$73:$72] by 2.
		lda $73
		asl $72
		rol a
		tax

		; add saved input * 3 value.
		lda $72
		clc
		adc $74
		sta $72
		txa
		adc $75
		tax
		lda $72
		rts

Now when we call multiply_xa_by_5, we must remember that we've used all the
zero-page locations from $70 up to $75. Any changes to either function must be
carefully checked in case variables are accidentally overwritten.

Handling this manually, whilst possible, can be quite error-prone. In Pasta,
this can be handled for you. You can write this:

		; Define automatically-allocated chunk of zero-page locations.
		.temps $70..$7f

		.context multiply_xa_by_3
		; a 2-byte variable allocated from the above pool.
		.var2 tmp
	multiply_xa_by_3:
		; original number in [x:a].
		sta %tmp
		stx %tmp + 1
		asl a
		rol %tmp + 1
		; now [%tmp1+1:a] are the original number * 2.
		clc
		adc %tmp
		sta %tmp
		txa
		adc %tmp + 1
		tax
		lda %tmp
		; now [x:a] should be the original number * 3.
		rts
		.ctxend

		.context multiply_xa_by_5
		; two 2-byte variables.
		.var2 tmp1, tmp2
	multiply_xa_by_5:
		sta %tmp1
		stx %tmp1 + 1
		jsr multiply_xa_by_3
		sta %tmp2
		stx %tmp2 + 1

		; multiply tmp1 by 2.
		lda %tmp1 + 1
		asl %tmp1
		rol a
		tax

		; add saved the input * 3 value in tmp2.
		lda %tmp1
		clc
		adc %tmp2
		sta %tmp1
		txa
		adc %tmp2 + 1
		tax
		lda %tmp1
		rts
		.ctxend

This generates code which is identically as fast as before, but we don't have to
worry about which zero-page locations we've used any more. Pasta notices that
multiply_xa_by_5 calls multiply_xa_by_3, so the "variables" (i.e. zero-page
locations) which the two functions use will not overlap.

Pasta supports a useful (though certainly not complete) set of familiar features
from other assemblers (macros, aliases, ...). At the moment there's only support
for single input files, and you can only output a raw binary.

Compiling
---------

Pasta is supplied as source code. Compile by typing 'make'. Prerequisites for
building include:

* OCaml (I'm using 3.11.2, other versions may work too).
* ocamldsort
* Menhir
* GNU Make

I've only tried building on Debian GNU/Linux. Other platforms will probably
work too.

Usage
-----

Assemble files (.s) with:

	pasta [options] <input file> -o <output file>

The output file format is raw binary.

Options are:

	-a : Write information about variable allocation to <basename>.alloc
	-q : Quiet (don't emit informational notes)

If "-o <output file>" is omitted, the binary name defaults to "a.out".

Syntax
------

The syntax supported by Pasta is quite typical for a 6502 assembler, and is
inspired mostly by xa65 and the Ophis assembler (though is not 100% compatible
with either). Write most opcodes in the usual style, one per-line, or separated
with a colon (which counts as a linefeed for parsing purposes):

	lda #65:sta $70

Instructions which use accumulator addressing (e.g. rol) can either specify 'a'
or not, e.g.:

	rol a

or just plain:

	rol

are both valid (and produce the same result).

Comments
--------

Comments are introduced with a ';' character, and last until the end of the
line. Comments may contain colons (i.e. colons will not end the comment).

Directives
----------

Assembler directives are introduced with a dot character, e.g.:

	.org $e00

Expressions
-----------

Pasta contains full support for evaluating assembly-time expressions.
Intermediate results are calculated with 32 bits of precision. Operators
supported are mostly C-like:

	+ (add)
	- (subtract)
	* (multiply)
	/ (divide)
	| (bitwise OR)
	^ (bitwise exclusive-OR)
	& (bitwise AND)
	~ (unary, bitwise NOT)
	- (unary minus)
	<< (left-shift)
	>> (logical right-shift)
	>>> (arithmetic right-shift)
	[ ] (brackets, for grouping -- note NOT normal () brackets!)
	< (unary, low byte)
	> (unary, high byte)

These have precedences as follows:

	(lowest)
	< >
	|
	^
	&
	<< >> >>>
	+ -
	* /
	(highest)

Immediates
----------

Write immediates using decimal:

	lda #65

In hexadecimal:

	lda #$41
or:

	lda #0x41

In binary:

	lda #0b01000001

Or as ASCII characters:

	lda #'A'

Program origin
--------------

Set the origin of the binary output using the ".org" directive:

	.org $e00

This affects e.g. jmp instructions which have an absolute destination address.

Labels
------

Labels are written on a line by themselves, e.g.:

	foo
		rts

Since colon counts as a line separator, the following also works:

	foo:
		rts

As does this:

	foo:	rts

But this does not work, since foo is not on a line by itself:

	foo	rts

Data
----

Constant data can be defined in your program using directives as follows:

Bytes:

	.byte 0xff, 0xfe

Words (two bytes):

	.word 0x1000, 0x2000

Triple-bytes:

	.3byte 0x123456, 0x7890ab

Doublewords (four bytes):

	.dword 0x12345678, 0x87654321

A block of memory:

	.dsb 32, 0

This will fill a block with 32 zero-bytes.

ASCII strings:

	.asc "hello world!"

Strings are *not* null-terminated. To terminate your strings, use e.g.:

	.asc "hello world!", 0

Escaping works properly in string immediates, using the \ character. E.g. you
can write:

	.asc "\"hello world\"", 0

Characters you can escape like this are:

	"
	'
	\
	n (newline)
	r (carriage return)
	t (tab)

If you need other non-printable characters, you can write e.g.:

	.asc "filename", 13

Branches
--------

Conditional branches are automatically lengthened (to a branch with the opposite
condition over a jmp instruction) when their target is out-of-scope. Pasta emits
a note when it does this.

Scopes
------

Labels defined in your program are visible globally, unless they are enclosed
within a "scope" as follows:

		.scope
		ldx #5
	loop:
		dex
		bne loop
		.scend

An alternative syntax (if you prefer) is:

		.(
		ldx #5
	loop:
		dex
		bne loop
		.)

Any label defined within a scope is only visible inside that scope (and in more
deeply-nested scopes). Forward references work as expected. There's currently no
way of "exporting" labels from within scopes.

Scopes allow you to reuse common label names (e.g. "loop"), without any problems
caused by multiple definitions.

If you're not using contexts, you might find it good practice to wrap each of
your functions with its own scope, like so:

	func1:
		.(
		...body of function...
		rts
		.)

Note that the entry point (func1) is outside the scope, so it will be visible
globally. Any labels defined inside the function will be invisible to the rest
of the program.

Include files
-------------

You can include other source files in your top-level assembly file using
the .include directive:

	.include "mydefs.s"

The result is as if the "mydefs.s" file was inserted directly into your
source file. The include file must be well-formed in its own right:
you cannot, for instance, open a macro definition in an include file
and close it in the top-level file, or vice versa.

Aliases
-------

You can define numerical aliases like so:

	.alias foo $4000

You can then use 'foo' in expressions, where its value will be substituted as if
you'd written $4000 directly. 

This can be useful e.g. for OS entry points:

	.alias oswrch $ffee
	
	[...]
	
	jsr oswrch

You can use an arbitrary expression as the RHS of an alias definition, e.g.

	.alias bar foo + 0x20

Macros
------

You can define macros with arguments as follows (note use of '%' to signify
macro arguments):

	.macro add res a b
	lda %a
	clc
	adc %b
	sta %res
	lda %a + 1
	adc %b + 1
	sta %res + 1
	.mend

Macros can then be invoked at arbitrary points in your program as follows:

	@add $70, $72, $74

Arguments are evaluated as numerical expressions, and substituted into the macro
expansion. (These are not C-style macros: you can't use them for arbitrary text
substitution.)

Internally, macros are expanded to scopes: this means that labels defined inside
a macro definition are local to each invocation of that macro.

Macro definitions cannot be nested within other definitions (which wouldn't make
much sense), but macro definitions can contain macro invocations. E.g. it's
legal to expand a macro inside a macro definition like this:

	.macro add_b_twice res a b
	@add %res, %a, %b
	@add %res, %res, %b
	.mend

There are currently no facilities for assembly-time conditionals or looping
constructs.

Passing addressing modes to macros
----------------------------------

A new feature in version 0.09 lets you pass arbitrary addressing modes to
macros. Macro definitions are unchanged:

	.macro add res a b
	lda %a
	clc
	adc %b
	sta %res
	.mend

As long as %a, %b and %res are written by themselves and not as part of a larger
expression or within some other addressing mode, the macro can be invoked as
follows:

	@add {$70}, {($72),y}, {($74),y}

This substitutes the given addresses into the macro expansion. The braces {} are
mandatory if you wish to use this feature, though if they are omitted then an
absolute address can still be substituted into the macro expansion, as you might
expect.

In the above case, the macro expansion will look like:

	lda ($72),y
	clc
	adc ($74),y
	sta $70

Note again that addresses can't be modified at all in the macro expansion: this
might limit the usefulness of this feature, in practice.

Contexts
--------

Contexts are the mechanism used to handle automatic zero-page management. They
are somewhat similar to scopes, but with extended functionality.

Several directives are concerned with contexts. Firstly you must specify which
zero-page locations are available for automatic management, using:

	.temps $70..$74

This would specify that locations $70 to $74 (inclusive) are available for
automatic allocation of variables. You can specify discontinuous regions, like
so:

	.temps $70..$74, $76, $78..$7a

A context definition looks like:

		.context foo
		.var a, b, c
	foo:
		lda %a
		sta %b
		...context body...
		rts
		.ctxend

Taking this apart a bit, the single word 'foo' after the context directive
defines an entry point for the context, and should correspond to a label defined
inside the context.

Variables may be 1, 2, 3 or 4 bytes in length. These are defined by directives,
e.g.:

	.var a    (a 1-byte variable 'a')
	.var2 b   (a 2-byte variable 'b')
	.var3 c   (a 3-byte variable 'c')
	.var4 d   (a 4-byte variable 'd')

Then %a, %b, %c and %d can be used in the context body, and will be substituted
by automatically-assigned zero-page locations.

Use, e.g.:

	lda %c + 2
	sta %d + 3

To refer to high bytes of variables. Don't offset beyond the size of the
variable, else unexpected things will happen (this is not diagnosed as an error
at present).

There are some limitations to the use of contexts. Inside a context, you may
only call or jump to an external label (one defined outside the current context)
if it was exported from a context itself: generally, control flow cannot
escape from a context into a non-context part of your program (although you can
safely jump *into* a context from anywhere).

There is one exception to this rule. If you want to call or jump to a label
which does not have a context around it, you can do so if you explicitly declare
that it uses none of the automatically-managed temporary space using the
".notemps" directive:

		.notemps oswrch

		.context print_a
	print_a:
		lda #65
		jsr oswrch
		rts
		.ctxend

If the "oswrch" function *does* use some of the temp variables though,
unexpected things will happen, so take care.

You cannot use conditional branches to escape from a context (though this is not
diagnosed as an error at present), nor can you use indirect jumps within a
context (code which does this will not assemble): Pasta must be able to
statically determine the call graph of your program. If you need to do more
complex control flow, you must arrange your program so that those bits are
not within a context.

You cannot define macros inside contexts, but of course macros can be invoked
inside contexts.

Advanced use of contexts
------------------------

It is permitted to access the variables defined inside contexts from elsewhere
in the program, if some care is taken. This enables a programming style where
arguments can be passed and returned in fast zero-page locations to/from
functions wrapped in contexts. As an example, a function can be defined as
follows:

		.context add
		.var res, a, b
	add:
		lda %a
		clc
		adc %b
		sta %res
		rts
		.ctxend

You can then call the function like this:

		.context my_program
		.var add_result
	my_program:
		lda #5
		sta %add.a
		lda #7
		sta %add.b
		jsr add
		lda %add.res
		sta %add_result
		rts
		.ctxend

Because 'add' is called from 'my_program', Pasta's zero-page allocator will
ensure that "add_result", "res", "a" and "b" are each allocated to different
locations.

Now, the complicated bit. If you call multiple contexts from a given context,
there is no guarantee that each callee's variables will all be distinct. (If
this wasn't so, we would run out of locations to allocate rather quickly). Say
we have the "add" context from above, and another similar one for subtraction:

		.context sub
		.var res, a, b
	sub:
		lda %a
		sec
		sbc %b
		sta %res
		rts
		.ctxend

		.context my_program
		.var add_result
		.var sub_result
	my_program:
		lda #5
		sta %add.a
		lda #7
		sta %add.b
		jsr add

		lda #15
		sta %sub.a
		; Uh-oh! "%sub.a" might have been the same as "%add.res"!
		lda %add.res
		sta %sub.b
		jsr sub
		lda %sub.res
		sta %sub_result
		rts
		.ctxend

Now: whereas %add.a, %add.b and %add.res will be distinct from %add_result,
%add.a, %add.b or %add.res may be the same as any of %sub.a, %sub.b or %sub.res.
So, when we try to read %add.res after writing to %sub.a, we might have
overwritten the value we wanted to use.

Pictorially, we have:

          +--------------+
          |  my_program  |
          +--------------+
           /            \
    +-------+          +-------+
    |  add  |          |  sub  |
    +-------+          +-------+

Now, variables from "my_program" interfere with variables from "add" (i.e. they
will be made to not overlap), and similarly variables from "my_program"
interfere with those from "sub", but the variables of "add" and "sub" (as
siblings) do not automatically interfere, so may be assigned to the same
locations by the allocator.

Of course, this is undesirable in this case, and unfortunately Pasta doesn't do
any real dataflow analysis (allocation is done at the granularity of whole
contexts), so can't tell by itself when this happens. So, if you want to use
variables from sibling contexts with overlapping ranges like this, you must
explicitly tell Pasta about it, to make sure the affected variables are
allocated to distinct locations. In the above program you should write:

		.context my_program
		.var sub_result
	my_program:
		lda #5
		sta %add.a
		lda #7
		sta %add.b
		jsr add

		; This will stop "%add.res" from being allocated to the same
		; location as a variable in any context which this context
		; calls.
		.protect %add.res

		lda #15
		sta %sub.a

		lda %add.res
		sta %sub.b
		jsr sub
		lda %sub.res
		sta %sub_result
		rts
		.ctxend

Only use ".protect" when necessary (as above), else you may find yourself
running out of zero-page locations too quickly. The "protection" acts for the
whole of the containing context.

You can write recursive functions using contexts, but you are responsible for
saving and restoring variables yourself (e.g. by pushing them onto a stack).

Running out of allocated space
------------------------------

So: how well does this work? As it turns out, apparently quite well in practice.
What we have, essentially, is the same thing as a call stack, but allocated
statically. As long as the maximum call depth of your program isn't too large,
and each of your functions doesn't use too many variables, and you make sure to
provide a fairly large number of zero-page bytes to the allocator (depending on
the complexity of your program of course), you'll probably be fine.

If you do happen to run out of space, Pasta will give you a message and exit:

	Ran out of temps!
	No space for: hello.i

In this case, your best options are to:

* Reduce the number of variables you are using (perhaps you can use a
non-zeropage location or the hardware stack for something instead?).

* Increase the number of temps in the allocation pool.

* Rearrange your program so the call stack is shallower.

It's possible that the temp pool could get fragmented in such a way that there
should be enough space for a particular multi-byte variable, but it is spread
around multiple locations in the pool rather than in one continuous chunk, since
variables are allocated "greedily". This hasn't proven to be a big issue in
practice for me at least, so far.

Links
-----

* xa65: http://www.floodgap.com/retrotech/xa/
* The Ophis Assembler: http://hkn.eecs.berkeley.edu/~mcmartin/ophis/

Contact
-------

Please send bug reports, general comments or whatever to:

jules "at" panic.cs-bristol.org.uk

Thanks!
