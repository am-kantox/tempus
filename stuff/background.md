# Mathematical Background

## Terminology 𝕋𝕥

The pair of two elements is called a `Slot` if the following conditions are met

- each element is either `nil` or an instance of a datetime with defined timezone
- if both elements are datetimes, the first element does not superseed the second one
- if either element is `nil`, the `Slot` is called _open_, if both are `nil`, it’s called _identity_

`Slot` id denoted `[from → to]`. Let’s define a binary _union_ operation on slots, denoted `∪`. Slots do not form a group with `∪`, but sorted sets of slots of arbitrary length having no joint slots (denoted `𝕥` or more verbose `𝕥[[from₁, to₁], [from₂, to₂], …]`) do indeed form a group, denoted `𝕋`, together with a binary operation `∪` on `𝕋`, such as the following group axioms are satisfied:

### Associativity

∀ 𝕥₁, 𝕥₂, 𝕥₃ ∈ 𝕋, (𝕥₁ ∪ 𝕥₂) ∪ 𝕥₃ = 𝕥₁ ∪ (𝕥₂ ∪ 𝕥₃)

### Identity element

∃ 𝕥₀ ∈ 𝕋 (`𝕥[]`) such that, for every 𝕥 in 𝕋, (𝕥 ∪ 𝕥₀) = (𝕥₀ ∪ 𝕥) = 𝕥

### Inverse element

For each 𝕥 in 𝕋, there exist 𝕥¯¹ such that 𝕥 ∪ 𝕥¯¹ = 𝕥¯¹ ∪ 𝕥 = 𝕥₀

That said, slots form an Abelian group with union binary operation and empty set as identity element.
## Slots Semigroup

Slots themselves form a semigroup with a binary union operation, an identity element `[nil → nil]`, without inverse.

## Binary Operation

∀ 𝕥₁ = 𝕥[[from₁ → to₁]], 𝕥₂ = 𝕥[[from₂ → to₂]] ∈ 𝕋, 𝕥₁ ∪ 𝕥₂ is defined as

- 𝕥[[from₁ → to₁], [from₂ → to₂]] if to₁ < from₂
- 𝕥[[from₂ → to₂], [from₁ → to₁]] if to₂ < from₁
- 𝕥[[min(from₁, from₂) → max(to₁ → to₂)]] otherwise

`nil` is considered to be less than any datetime _and_ greater than any datetime, thus `𝕥[[nil → to₁]] ∪ 𝕥[[from₂ → to₂]]` would be either `𝕥[[nil → max(to₁, to₂)]]` if `to₁ > from₂` or `𝕥[[nil → to₁], [from₂ → to₂]]` otherwise.

## Mergeability

Once `𝕋` is a group, each two elements of it _might be merged_. Even if they are infinite. That’s why `Stream` implementation of `𝕋` exists.