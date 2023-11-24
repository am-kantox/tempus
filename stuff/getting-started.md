# Getting Started

## Usecases

`Tempus` library provides an easy way to handle and manipulate time slots.

Time slot is the base concept behind the library. It is basically the struct, containing `from` and `to` instances of `DateTime`. One might provide the list of slots, as `Enum` or as a `Stream`, and check whether another time slot and/or `DateTime` instance is overlapped or disjoined. Also one might get the next “available” slot(s) providing an `origin`, as well as next busy slot(s).

It might be used e. g. to calculate the working days between now and some day in the future, providing the list of holidays, and the stream of weekends.

## Initializing Slots

`Tempus.Slots` has an agnostic internal-storage interface for keeping slots. It might have a `List` or `Stream` behind, a self-baked implementation of `Tempus.Group` protocol, ot it might leverage [`AVLTree`](https://github.com/japplegame/avl_tree). It exposes `add/2`, `merge/2` and `inverse/2` functions to add slots, merge the slots with another slot(s) and/or `Stream`, and inverse the slots respecively. Typically one starts with something like

```elixir
slots = [
  Tempus.Slot.wrap(~D|2020-08-07|),
  Tempus.Slot.wrap(~D|2020-08-10|),
  %Tempus.Slot{
      from: ~U|2020-08-07 01:00:00Z|, to: ~U|2020-08-08 01:00:00Z|}
] |> Enum.into(%Tempus.Slots{})
```

When updated, `Tempus.Slots` joins the newly added slot(s) into the set of existing ones, so thet at any moment there is no overlapped slots. This happens when new slot is added with `Tempus.Slots.add/2` and when the enumerable is collected.

### Consuming Streams

`Tempus.Slots` is optimized for reads, that’s why internally it keeps slots as an `Enum`. It only allows to merge `Stream` into the existing `Tempus.Slots` and the stream will be terminated within the range of the `Enum` already presented. Below is the example of consuming the stream.

```elixir
holidays = [~D|2020-08-06|, ~D|2020-08-13|]
weekends = Stream.map([~D|2020-08-08|, ~D|2020-08-20|], & &1)
schedule = holidays |> Enum.into(%Slots{}) |> Slots.merge(weekends)

#⇒ #Slots<[
#    #Slot<[
#      from: ~U[2020-08-06 00:00:00.000000Z],
#      to: ~U[2020-08-06 23:59:59.999999Z]
#    ]>,
#    #Slot<[
#      from: ~U[2020-08-08 00:00:00.000000Z],
#      to: ~U[2020-08-08 23:59:59.999999Z]
#    ]>,
#    #Slot<[
#      from: ~U[2020-08-13 00:00:00.000000Z],
#      to: ~U[2020-08-13 23:59:59.999999Z]
#    ]>
#  ]>
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
