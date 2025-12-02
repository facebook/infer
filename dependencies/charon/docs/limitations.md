# Current limitations of Charon

Charon is alpha software. In particular, it is currently poorly documented, doesn't support all the
Rust features we'd like, and has several breaking changes planned in the near future.

## Planned breaking changes

- https://github.com/AeneasVerif/charon/issues/287
- https://github.com/AeneasVerif/charon/issues/194
- https://github.com/AeneasVerif/charon/issues/582
- Some potential rework of how we handle builtins like `SliceIndexMut`
- Name matcher behavior likely to change in subtle ways
  (https://github.com/AeneasVerif/charon/issues/319).

## Known unsoundnesses

- https://github.com/AeneasVerif/charon/issues/583
- https://github.com/AeneasVerif/charon/issues/584

## Unsupported Rust features

Tracked here: https://github.com/AeneasVerif/charon/issues/142

## Missing information in the translated output

- Bodies of functions in foreign crates often cause errors (https://github.com/AeneasVerif/charon/issues/543);
- Bodies of std functions (https://github.com/AeneasVerif/charon/issues/545);
- Drops (https://github.com/AeneasVerif/charon/issues/152);
- Layout information (https://github.com/AeneasVerif/charon/issues/581);
- Lifetime information inside function bodies (not planned).
