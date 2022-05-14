alias Tempus.{Slot, Slots}

micros_in_week = 604_800_000_000

first_weekend = %Slot{from: ~U|2018-01-05 21:00:00Z|, to: ~U|2018-01-08 08:59:59Z|}

weekends =
  Stream.iterate(
    first_weekend,
    fn acc ->
      acc
      |> Tempus.Slot.shift(from: micros_in_week, to: micros_in_week)
      |> Tempus.Slot.shift_tz()
    end
  )

slots =
  [~D|2018-01-01|, ~D|2022-12-31|]
  |> Enum.into(%Slots{})
  |> Slots.merge(weekends)

Benchee.run(%{
  :add_plus => fn -> Tempus.add(slots, ~U|2020-09-13 12:00:00Z|, 600) end,
  :add_minus => fn -> Tempus.add(slots, ~U|2020-09-13 12:00:00Z|, -600) end,
  :size_free? => fn -> Tempus.free?(slots, ~U|2020-09-13 12:00:00Z|, :size) end,
  :smart_free? => fn -> Tempus.free?(slots, ~U|2020-09-13 12:00:00Z|, :smart) end,
  :inverse => fn -> Slots.inverse(slots) end,
  :next_busy => fn -> Tempus.next_busy(slots, origin: ~D|2020-09-13|) end,
  :next_free => fn -> Tempus.next_free(slots, origin: ~D|2020-09-13|) end
})
