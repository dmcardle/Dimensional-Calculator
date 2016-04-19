# Dimensional Calculator

The main idea here is to make a type checker for dimensional analysis.  It should behave as a normal command-line calculator like `bc`, but accept units for each quantity.  When the user has asked for something impossible, like adding two quantities of different units, it should produce an error message. When the user enters a correctly-labeled calculation, both the numerical result and the simplified units should be shown.

## Examples of Planned Behavior

    5 apples + 3 apples
    => 8 apples

    5 apples * 3 apples
    => 15 apples^2

    5 apples + 3 bananas
    => [Dimensional Error]
