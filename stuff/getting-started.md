# Getting Started

## Usecases

`Tempus` library provides an easy way to handle and manipulate time slots. One might provide the list of slots, as `Enum` or as a `Stream`, and check whether another time slot and/or `DateTime` instance is overlapped or disjoined. Also one might get the next “available” slot(s) providing an `origin`, as well as next busy slot(s).

It might be used e. g. to calculate the working days between now and some day in the future, providing the list of holidays, and the stream of weekends.

## Initializing Slots

`Tempus.Slots` module leverages [`AVLTree`](https://github.com/japplegame/avl_tree). It exposes `add/2`, `merge/2` and `inverse/2` functions to add slots, merge the slots with another slot(s) and/or `Stream`, and inverse the slots respecively. Typically one starts with something like

```elixir
slots = [
  Tempus.Slot.wrap(~D|2020-08-07|),
  Tempus.Slot.wrap(~D|2020-08-10|),
  %Tempus.Slot{
      from: ~U|2020-08-07 01:00:00Z|, to: ~U|2020-08-08 01:00:00Z|}
] |> Enum.into(%Tempus.Slots{})
```

## Checking State

With a help of `Tempus.next_busy/2` and `Tempus.next_free/2`, one might get the next busy and/or free slot(s). To calculate the first free slot in 5 business days after `2020-08-05`, the following is to be done.

```elixir
holidays = [~D|2020-08-07|, ~D|2020-08-13|] # ...
weekends = [~D|2020-08-08|, ~D|2020-08-09|] # ...

holidays
|> Enum.into(%Slots{})
|> Tempus.Slots.merge(weekends)
|> Tempus.days_ahead(~D|2020-08-05|, 5)
|> hd()
#⇒ "2020-08-14"
```
