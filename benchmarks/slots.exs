alias Tempus.{Slot, Slots}

micros_in_week = 604_800_000_000

first_weekend = %Slot{from: ~U|2018-01-05 21:00:00Z|, to: ~U|2018-01-08 08:59:59Z|}
first_monday = Slot.wrap(~D|2018-01-08|)

weekends =
  Stream.iterate(
    first_weekend,
    fn acc ->
      acc
      |> Tempus.Slot.shift(from: micros_in_week, to: micros_in_week)
      |> Tempus.Slot.shift_tz()
    end
  )

mondays =
  Stream.iterate(
    first_monday,
    fn acc ->
      acc
      |> Tempus.Slot.shift(from: micros_in_week, to: micros_in_week)
      |> Tempus.Slot.shift_tz()
    end
  )

slots = Enum.into([~D|2018-01-01|, ~D|2025-12-31|], %Slots{})
weekend_slots = Slots.merge(slots, weekends)
monday_slots = Slots.merge(slots, mondays)

Benchee.run(%{
  :add_plus => fn -> Tempus.add(weekend_slots, ~U|2020-09-13 12:00:00Z|, 600) end,
  :add_minus => fn -> Tempus.add(weekend_slots, ~U|2020-09-13 12:00:00Z|, -600) end,
  :size_free? => fn -> Tempus.free?(weekend_slots, ~U|2020-09-13 12:00:00Z|, :size) end,
  :smart_free? => fn -> Tempus.free?(weekend_slots, ~U|2020-09-13 12:00:00Z|, :smart) end,
  :inverse => fn -> Slots.inverse(weekend_slots) end,
  :next_busy => fn -> Tempus.next_busy(weekend_slots, origin: ~D|2020-09-13|) end,
  :next_free => fn -> Tempus.next_free(weekend_slots, origin: ~D|2020-09-13|) end,
  :merge => fn -> Slots.merge(weekend_slots, monday_slots) end,
  :slice => fn -> Tempus.slice(weekend_slots, ~U[2020-08-08 12:00:00.000000Z], ~U[2020-08-12 12:00:00.000000Z]) end
})
