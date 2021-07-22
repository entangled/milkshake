# Tutorial
Milkshake uses the Dhall language to specify dependencies between actions. In the most basic operation, we have `Target`s and `Action`s. A `Target` is usually one or more files, while an `Action` is a shell script that would generate said `Target`s, after satisfying a set of dependencies (also `Target`s).

The resulting configuration is a list of `Action`s.

``` {.dhall}

```