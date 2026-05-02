# Stats Views — Motivation

## The audience

Grad students in psychology, biology, and social sciences doing thesis stats. Stats 101 undergrads as a secondary audience. Defaults are shaped by the thesis writer — that's the user we relate to best.

These users want to run a t-test, fit a regression, run an ANOVA. They want a publication-ready table or paragraph at the other end. They don't want to learn R syntax, debug package installation, or fight an IDE. They reach for SPSS, jamovi, or JASP because those tools hide the syntax and produce formatted output by clicking through menus.

## What we tried, what we learned

We first tried wrapping `jmv` (jamovi's R package) as a thin layer of blockr blocks emitting tidy tibbles. That failed: extracting one tibble from a `jmv::*` call discards what makes jamovi impressive (multi-table coordination, formatted output, plots together), and the result is something you could build in 30 minutes wrapping `psych::describe()`. The exercise also surfaced a deeper finding: the SPSS UX (menu → dialog → output viewer) and the default blockr UX (drag node, wire pipeline) are different mental models. Adding atomic blocks doesn't bridge them.

The reframe that came out of that: **we're not trying to be SPSS or replace jamovi — those products exist and serve their audience.** We're targeting the same audience with a different shape that benefits a slice of them on real wedges, not all of them on every dimension.

## The shape: views, not blocks

Students don't wire blocks. They open a **pre-configured view** for the task they want — data exploration, modeling, t-tests — and the view is a complete workflow with everything wired and laid out. The student picks variables, sees output, goes home. Same speed as jamovi for the basic case.

The block layer underneath exists for extension and customization. A power user opens it when the pre-built view doesn't cover what they need — adding more blocks, or dropping in a `function_block` to wrap arbitrary R code without authoring a new block. Students never have to. The mechanism for shipping these views is a design-phase decision (a script, a saved board state, a function returning a board) — what matters at the motivation level is that **the student's unit of interaction is the view, not the block**.

## Why a thesis writer would pick this over jamovi

Honest position: we're better than jamovi for the slice of students who benefit from at least one of these wedges. For a clean dataset and a one-off t-test with no aspirations beyond the thesis defense, jamovi is the right tool.

- **Canonical R code generation.** Every block exposes the R it produces. The code is the R a working data scientist would write — `lm(...)`, `dplyr::summarise(...)`, `broom::tidy(...)`, `parameters::model_parameters(...)`. A student can copy our code into a script and it runs unchanged outside our app. jamovi exports R that uses `jmv::descriptives()` and similar — useless if you want to learn real R or hand the analysis to a collaborator.
- **Composability with the broader blockr ecosystem.** jamovi has trivial filtering and variable recoding; anything beyond that — joins, pivots, cross-filter, derived columns — sends the student to R. blockr already has these as blocks (blockr.dplyr, cross-filter, etc.). The student does the data prep AND the analysis in one workspace.
- **Graduation ladder built in.** The ladder has four rungs, each visible from the same tool:
    1. **View user.** Picks a pre-configured view, configures variables, sees output. No code, no wiring.
    2. **Composer.** Stays in the block UI. Drags additional blocks (filter, mutate, plot) into the view to extend it. Still no code.
    3. **Power user.** Drops a `function_block` into the view (`blockr.extra`) and writes a few lines of R inside it — fits a custom model, runs a non-standard test, calls a package we don't yet wrap. Real R, but additive: no new package, no new block to author.
    4. **Block developer.** Writes a proper blockr block with UI, state, and serialization, contributing back when something the power user did is worth sharing.

    Each rung is a strict superset of the one below. The student can stay on rung 1 forever. The R-curious student moves to rung 3 the day they need something the views don't cover, and reads the R the views are already generating along the way. jamovi/SPSS hide the code behind menus; the on-ramp to real R is "leave jamovi." Here it's a step inside the same tool.
- **Deployable Shiny artifact.** Less relevant to the individual student, more to a partner or institution. A blockr workspace is a Shiny app you can deploy, host, embed. A jamovi analysis lives in a `.omv` file on someone's laptop.

## Catching up is engineering, not architecture

Where jamovi clearly wins today: years of UX polish on analysis dialogs (variable role pickers, default-value muscle memory) and feature breadth (Bayesian variants, factor analysis, reliability, etc.). We won't match either in the prototype.

The bet behind this spec: **catch-up is tractable engineering work, not a deal-breaker.** UX polish on a small set of analysis types is finite. Feature breadth comes from extending one adaptive model block to cover linear, logistic, multinomial, ordinal, and mixed-effects models — all share enough structurally to fit one block, with output blocks polymorphing over the model object via easystats / broom. The prototype hits jamovi-quality on each of three views end-to-end (modeling, data exploration, t-tests), with the modeling view as the polish template the others copy.

## Why now

A partner team has shown interest in exploring this direction with us. The prototype gives the conversation something concrete to react to: a working view that demonstrates the wedges, plus enough breadth elsewhere to suggest the catch-up is real. Both sides learn whether this lands.
