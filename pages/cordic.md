---
title: Computing sin & cos with synthesisable Verilog
---

The [CORDIC][cordic] algorithm is a clever method for accurately computing
trigonometric functions using only additions, bitshifts and a small lookup table.

## The Algorithm

It's well known that rotating the vector $(1, 0)$ anticlockwise about the
origin by an angle $\theta$ gives the vector $(\cos \theta, \sin \theta)$. We
will use this as the basis of our algorithm:

    function cordic(θ) {
        [c, s] ← rotate([1, 0], θ)
        return c, s
    }

Let's split up this big rotation into $N$ smaller rotations, with the angle of
rotation in step $i$ given by $\alpha_i$. At the moment, we don't care what the
values of $\alpha_i$ are, we just want their sum to be equal to $\theta$.

    α ← [
        ...
    ]

    function cordic(θ) {
        c ← 1
        s ← 0
        for i in 0 .. N-1 {
            [c, s] ← rotate([c, s], α[i])
        }
        return c, s
    }

How do we rotate a vector? [The page for rotation on Wikipedia][rotation] tells
us that it is equivalent to left-multiplying by a particular matrix:

    α ← [
        ...
    ]

    function cordic(θ) {
        c ← 1
        s ← 0
        for i in 0 .. N-1 {
            rotation_matrix ← [[cos α[i], -sin α[i]],
                               [sin α[i],  cos α[i]]]
            [c, s] ← rotation_matrix × [c, s]
        }
        return c, s
    }

Let's expand out this matrix multiplication:

    α ← [
        ...
    ]

    function cordic(θ) {
        c ← 1
        s ← 0
        for i in 0 .. N-1 {
            c_new ← cos α[i] × c - sin α[i] × s
            s_new ← sin α[i] × c + cos α[i] × s
            c ← c_new
            s ← s_new
        }
        return c, s
    }

We will use the fact that $\sin \alpha_i = \cos \alpha_i \tan \alpha_i$ to
factorise by $\cos \alpha_i$:

    α ← [
        ...
    ]

    function cordic(θ) {
        c ← 1
        s ← 0
        for i in 0 .. N-1 {
            c_new ← cos α[i] × (c - tan α[i] × s)
            s_new ← cos α[i] × (s + tan α[i] × c)
            c ← c_new
            s ← s_new
        }
        return c, s
    }

Now, we will consider the values of $\alpha_i$. Let's assume that each rotation
angle $\alpha_i$ has a fixed magnitude $\beta_i$ with respect to $\theta$, but
can be either positive or negative depending on the value of $\theta$. In this
way, the rotating vector can be directed to converge on a particular result
vector.

We will accomplish this using a new variable $\phi$, initially at 0. Each step,
we compare $\phi$ to $\theta$. If $\phi$ is less than $\theta$, we need to
rotate by a positive angle (i.e. $\alpha_i = \beta_i$). If $\phi$ is greater
than $\theta$, we need to rotate by a negative angle ($\alpha_i = -\beta_i$).

    β ← [
        ...
    ]

    function cordic(θ) {
        c ← 1
        s ← 0
        φ ← 0
        for i in 0 .. N-1 {
            if φ < θ {
                direction ← 1
            } else {
                direction ← -1
            }
            α ← direction × β[i]
            c_new ← cos α × (c - tan α × s)
            s_new ← cos α × (s + tan α × c)
            c ← c_new
            s ← s_new
            φ ← φ + α
        }
        return c, s
    }

Let's eliminate the variable $\alpha$ and use $\beta_i$ directly:

    β ← [
        ...
    ]

    function cordic(θ) {
        c ← 1
        s ← 0
        φ ← 0
        for i in 0 .. N-1 {
            if φ < θ {
                direction ← 1
            } else {
                direction ← -1
            }
            c_new ← cos (direction × β[i]) × (c - tan (direction × β[i]) × s)
            s_new ← cos (direction × β[i]) × (s + tan (direction × β[i]) × c)
            c ← c_new
            s ← s_new
            φ ← φ + (direction × β[i])
        }
        return c, s
    }

$\cos (-x) = \cos x$ and $\tan (-x) = -\tan x$, so this program is equivalent to:

    β ← [
        ...
    ]

    function cordic(θ) {
        c ← 1
        s ← 0
        φ ← 0
        for i in 0 .. N-1 {
            if φ < θ {
                direction ← 1
            } else {
                direction ← -1
            }
            c_new ← cos β[i] × (c - direction × tan β[i] × s)
            s_new ← cos β[i] × (s + direction × tan β[i] × c)
            c ← c_new
            s ← s_new
            φ ← φ + (direction × β[i])
        }
        return c, s
    }

Now what about the values of $\beta_i$? If we assign them to be such that
$\tan \beta_i = 2^{-i}$, then we have:

    β ← [
        atan 2^0,
        atan 2^1,
        ...
        atan 2^(N-1)
    ]

    function cordic(θ) {
        c ← 1
        s ← 0
        φ ← 0
        for i in 0 .. N-1 {
            if φ < θ {
                direction ← 1
            } else {
                direction ← -1
            }
            c_new ← cos β[i] × (c - direction × 2^(-i) × s)
            s_new ← cos β[i] × (s + direction × 2^(-i) × c)
            c ← c_new
            s ← s_new
            φ ← φ + (direction × β[i])
        }
        return c, s
    }

Multiplication by $2^{-i}$ is a shift-right by $i$ places:

    β ← [
        atan 2^0,
        atan 2^1,
        ...
        atan 2^(N-1)
    ]

    function cordic(θ) {
        c ← 1
        s ← 0
        φ ← 0
        for i in 0 .. N-1 {
            if φ < θ {
                direction ← 1
            } else {
                direction ← -1
            }
            c_new ← cos β[i] × (c - direction × (s >> i))
            s_new ← cos β[i] × (s + direction × (c >> i))
            c ← c_new
            s ← s_new
            φ ← φ + (direction × β[i])
        }
        return c, s
    }

Let's move the multiplications by $\cos \beta_i$ into a separate loop:

    β ← [
        atan 2^0,
        atan 2^1,
        ...
        atan 2^(N-1)
    ]

    function cordic(θ) {
        c ← 1
        s ← 0
        φ ← 0
        for i in 0 .. N-1 {
            if φ < θ {
                direction ← 1
            } else {
                direction ← -1
            }
            c_new ← c - direction × (s >> i)
            s_new ← s + direction × (c >> i)
            c ← c_new
            s ← s_new
            φ ← φ + (direction × β[i])
        }
        for i in 0 .. N-1 {
            c ← c × cos β[i]
            s ← s × cos β[i]
        }
        return c, s
    }

In fact, all these multiplications can be replaced with a single value, which
we will call $K$. $K$ is equal to the product of $\cos \beta_i$ for all $i$
between $0$ and $N-1$ inclusive. To put it mathematically,

$$K = \prod_{i=0}^{N-1} \cos \beta_i$$

It can be shown through the use of trigonometric identities that:

$$K = \prod_{i=0}^{N-1} \frac{1}{\sqrt{1 + 2^{2i}}} = 0.607252935008881\dots$$

Therefore, we have:

    β ← [
        atan 2^0,
        atan 2^1,
        ...
        atan 2^(N-1)
    ]
    
    K ← 1
    for i in 0 .. N-1 {
        K ← K × cos β[i]
    }

    function cordic(θ) {
        c ← 1
        s ← 0
        φ ← 0
        for i in 0 .. N-1 {
            if φ < θ {
                direction ← 1
            } else {
                direction ← -1
            }
            c_new ← c - direction × (s >> i)
            s_new ← s + direction × (c >> i)
            c ← c_new
            s ← s_new
            φ ← φ + (direction × β[i])
        }
        c ← c × K
        s ← s × K
        return c, s
    }

Finally, we will change $\phi$ so that it now holds $\theta -$ whatever it was
holding before this change. This replaces the comparison against $\theta$ with a
comparison against $0$, which is simpler to compute:

    β ← [
        atan 2^0,
        atan 2^1,
        ...
        atan 2^(N-1)
    ]
    
    K ← 1
    for i in 0 .. N-1 {
        K ← K × cos β[i]
    }

    function cordic(θ) {
        c ← 1
        s ← 0
        φ ← θ
        for i in 0 .. N-1 {
            if φ > 0 {
                direction ← 1
            } else {
                direction ← -1
            }
            c_new ← c - direction × (s >> i)
            s_new ← s + direction × (c >> i)
            c ← c_new
            s ← s_new
            φ ← φ - (direction × β[i])
        }
        c ← c × K
        s ← s × K
        return c, s
    }

What we now have is an algorithm that is possible to implement in hardware, but
is still equivalent to the original algorithm.

## The implementation

We will assume that all numbers are stored as 32-bit fixed-point numbers, with
the radix point between the second-most-significant and third-most-significant
bits. This allows for numbers ranging between -2 and 2, which includes the
interval $-\frac{\pi}{2}$ to $\frac{\pi}{2}$ that all other inputs can be
reduced into.

First off, we declare the constants that we will use:

```verilog
`define K 32'h26dd3b6a  // = 0.6072529350088814

`define BETA_0  32'h3243f6a9  // = atan 2^0     = 0.7853981633974483
`define BETA_1  32'h1dac6705  // = atan 2^(-1)  = 0.4636476090008061
`define BETA_2  32'h0fadbafd  // = atan 2^(-2)  = 0.24497866312686414
`define BETA_3  32'h07f56ea7  // = atan 2^(-3)  = 0.12435499454676144
`define BETA_4  32'h03feab77  // = atan 2^(-4)  = 0.06241880999595735
`define BETA_5  32'h01ffd55c  // = atan 2^(-5)  = 0.031239833430268277
`define BETA_6  32'h00fffaab  // = atan 2^(-6)  = 0.015623728620476831
`define BETA_7  32'h007fff55  // = atan 2^(-7)  = 0.007812341060101111
`define BETA_8  32'h003fffeb  // = atan 2^(-8)  = 0.0039062301319669718
`define BETA_9  32'h001ffffd  // = atan 2^(-9)  = 0.0019531225164788188
`define BETA_10 32'h00100000  // = atan 2^(-10) = 0.0009765621895593195
`define BETA_11 32'h00080000  // = atan 2^(-11) = 0.0004882812111948983
`define BETA_12 32'h00040000  // = atan 2^(-12) = 0.00024414062014936177
`define BETA_13 32'h00020000  // = atan 2^(-13) = 0.00012207031189367021
`define BETA_14 32'h00010000  // = atan 2^(-14) = 6.103515617420877e-05
`define BETA_15 32'h00008000  // = atan 2^(-15) = 3.0517578115526096e-05
`define BETA_16 32'h00004000  // = atan 2^(-16) = 1.5258789061315762e-05
`define BETA_17 32'h00002000  // = atan 2^(-17) = 7.62939453110197e-06
`define BETA_18 32'h00001000  // = atan 2^(-18) = 3.814697265606496e-06
`define BETA_19 32'h00000800  // = atan 2^(-19) = 1.907348632810187e-06
`define BETA_20 32'h00000400  // = atan 2^(-20) = 9.536743164059608e-07
`define BETA_21 32'h00000200  // = atan 2^(-21) = 4.7683715820308884e-07
`define BETA_22 32'h00000100  // = atan 2^(-22) = 2.3841857910155797e-07
`define BETA_23 32'h00000080  // = atan 2^(-23) = 1.1920928955078068e-07
`define BETA_24 32'h00000040  // = atan 2^(-24) = 5.960464477539055e-08
`define BETA_25 32'h00000020  // = atan 2^(-25) = 2.9802322387695303e-08
`define BETA_26 32'h00000010  // = atan 2^(-26) = 1.4901161193847655e-08
`define BETA_27 32'h00000008  // = atan 2^(-27) = 7.450580596923828e-09
`define BETA_28 32'h00000004  // = atan 2^(-28) = 3.725290298461914e-09
`define BETA_29 32'h00000002  // = atan 2^(-29) = 1.862645149230957e-09
`define BETA_30 32'h00000001  // = atan 2^(-30) = 9.313225746154785e-10
`define BETA_31 32'h00000000  // = atan 2^(-31) = 4.656612873077393e-10
```

Then, we introduce the module and its ports.

```verilog
module cordic(
    angle,
    
    clock,    // Master clock
    reset,    // Master asynchronous reset (active-high)
    start,    // An input signal that the user of this module should set to high when computation should begin
    angle_in, // Input angle
    cos_out,  // Output value for cosine of angle
    sin_out   // Output value for sine of angle
);

input clock;
input reset;
input start;
input [31:0] angle_in;
output [31:0] cos_out;
output [31:0] sin_out;

output [31:0] angle;

wire [31:0] cos_out = cos;
wire [31:0] sin_out = sin;
```

We need five registers: `cos`, `sin` and `angle` from the above algorithm, a
counter register `count`, and a state register `state`. The first three are 32
bits wide, since they are storing fixed-point numbers as described above.
`count` is 5 bits since 32 iterations will be used, and `state` is only 1 bit
since we only need two states.

The inputs to this registers will be determined in a
sequential logic block, so five more registers `cos_next`, `sin_next`,
`angle_next`, `count_next` and `state_next` are defined, although these will be
automatically reduced to logic functions during synthesis.

```verilog
reg [31:0] cos;
reg [31:0] sin;
reg [31:0] angle;
reg [4:0] count;
reg state;

reg [31:0] cos_next;
reg [31:0] sin_next;
reg [31:0] angle_next;
reg [4:0] count_next;
reg state_next;

always @(posedge clock or posedge reset) begin
    if (reset) begin
        cos <= 0;
        sin <= 0;
        angle <= 0;
        count <= 0;
        state <= 0;
    end else begin
        cos <= cos_next;
        sin <= sin_next;
        angle <= angle_next;
        count <= count_next;
        state <= state_next;
    end
end
```

The two states that we will use are `0`, an idle state, and `1`, a state
indicating computation is occurring.

The logic block is defined as follows:

```verilog
always @* begin
    // Set all logic regs to a value to prevent any of them holding the value
    // from last tick and hence being misinterpreted as hardware registers.
    cos_next = cos;
    sin_next = sin;
    angle_next = angle;
    count_next = count;
    state_next = state;
    
    if (state) begin
        // Compute mode.
        cos_next = cos + (direction_negative ? sin_shr : -sin_shr);
        sin_next = sin + (direction_negative ? -cos_shr : cos_shr);
        angle_next = angle + (direction_negative ? beta : -beta);
        count_next = count + 1;
        
        if (count == 31) begin
            // If this is the last iteration, go back to the idle state.
            state_next = 0;
        end
    end
    
    else begin
        // Idle mode.
        if (start) begin
            cos_next = `K;         // Set up initial value for cos.
            sin_next = 0;          // Set up initial value for sin.
            angle_next = angle_in; // Latch input angle into the angle register.
            count_next = 0;        // Set up counter.
            state_next = 1;        // Go to compute mode.
        end
    end
end
```

The `cos_shr` and `sin_shr` signals refer to the values of `cos` and `sin`
shifted right by `count` places:

```verilog
wire [31:0] cos_signbits = {32{cos[31]}};
wire [31:0] sin_signbits = {32{sin[31]}};
wire [31:0] cos_shr = {cos_signbits, cos} >> count;
wire [31:0] sin_shr = {sin_signbits, sin} >> count;
```

The `direction_negative` signal is high if `direction` in the pseudocode
algorithm is negative (if $φ < 0$), and low if it is positive (if $φ ≥ 0$). It
is therefore defined to simply be equal to the sign bit of `angle`:

```verilog
wire direction_negative = angle[31];
```

Lastly, we will define the lookup table used for the `beta` values:

```verilog
wire [31:0] beta_lut [0:31];
assign beta_lut[0] = `BETA_0;
assign beta_lut[1] = `BETA_1;
// ...
assign beta_lut[31] = `BETA_31;

wire [31:0] beta = beta_lut[count];
```

The complete source code can be downloaded here:

* [`cordic.v`](/static/download/cordic.v)
* [`cordic_test.v`](/static/download/cordic_test.v)

[cordic]: http://en.wikipedia.org/wiki/CORDIC
[rotation]: http://en.wikipedia.org/wiki/Rotation_%28mathematics%29#Two_dimensions
