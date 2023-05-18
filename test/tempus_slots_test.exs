defmodule Tempus.Slots.Test do
  use ExUnit.Case, async: false
  doctest Tempus.Slots
  doctest Tempus.Slots.List
  doctest Tempus.Slots.Stream

  alias Tempus.{Slot, Slots}
  import Tempus.Sigils

  setup_all do
    _micros_in_hour = 3_600_000_000
    _micros_in_day = 86_400_000_000

    day_after_day_stream =
      Slots.Stream.iterate(~D|2023-01-01|, &Slot.shift(&1, by: 2, unit: :day))

    day_after_day_list = Enum.take(day_after_day_stream, 10)

    [day_after_day_stream: day_after_day_stream, day_after_day_list: day_after_day_list]
  end

  describe "List.Group" do
    setup ctx do
      [input: ctx.day_after_day_list, slots: Slots.new(:list, ctx.day_after_day_list)]
    end

    test "identity/1", %{slots: slots} do
      assert %Slots{slots: %Slots.List{slots: []}} = Slots.identity(slots)
    end

    test "flatten/1", %{input: input, slots: slots} do
      assert Enum.map(input, &Slot.wrap/1) == Slots.flatten(slots)
    end

    test "add/3", %{input: _input, slots: slots} do
      assert %Slots{
               slots: %Slots.List{
                 slots: [
                   ~I(2023-01-01T00:00:00.000000Z → 2023-01-05T23:59:59.999999Z),
                   ~I(2023-01-07T00:00:00.000000Z → 2023-01-07T23:59:59.999999Z) | _
                 ]
               }
             } = Slots.add(slots, ~I|2023-01-02 → 2023-01-04|d, join: true)

      assert %Slots{
               slots: %Slots.List{
                 slots: [
                   ~I(2023-01-01T00:00:00.000000Z → 2023-01-01T23:59:59.999999Z),
                   ~I(2023-01-02T00:00:00.000000Z → 2023-01-04T23:59:59.999999Z) | _
                 ]
               }
             } = Slots.add(slots, ~I|2023-01-02 → 2023-01-04|d, join: false)
    end

    test "merge/3", %{input: _input, slots: slots} do
      assert %Slots{
               slots: %Slots.List{
                 slots: [
                   ~I(2023-01-01T00:00:00.000000Z → 2023-01-05T23:59:59.999999Z),
                   ~I(2023-01-07T00:00:00.000000Z → 2023-01-07T23:59:59.999999Z) | _
                 ]
               }
             } = Slots.merge(slots, Slots.wrap(~I|2023-01-02 → 2023-01-04|d), join: true)

      assert %Slots{
               slots: %Slots.List{
                 slots: [
                   ~I(2023-01-01T00:00:00.000000Z → 2023-01-01T23:59:59.999999Z),
                   ~I(2023-01-02T00:00:00.000000Z → 2023-01-04T23:59:59.999999Z) | _
                 ]
               }
             } = Slots.merge(slots, Slots.wrap(~I|2023-01-02 → 2023-01-04|d), join: false)
    end
  end

  describe "Stream.Group" do
    setup ctx do
      [input: ctx.day_after_day_stream, slots: %Slots{slots: ctx.day_after_day_stream}]
    end

    test "identity/1", %{slots: slots} do
      assert %Slots{slots: %Slots.Stream{}} = Slots.identity(slots)
    end

    test "flatten/1", %{input: input, slots: slots} do
      assert input |> Stream.map(&Slot.wrap/1) |> Enum.take(3) == Slots.flatten(slots, until: 3)
    end

    test "add/3", %{input: _input, slots: slots} do
      assert [
               ~I(2023-01-01T00:00:00.000000Z → 2023-01-05T23:59:59.999999Z),
               ~I(2023-01-07T00:00:00.000000Z → 2023-01-07T23:59:59.999999Z) | _
             ] = Slots.add(slots, ~I|2023-01-02 → 2023-01-04|d, join: true) |> Enum.take(2)

      assert [
               ~I(2023-01-01T00:00:00.000000Z → 2023-01-01T23:59:59.999999Z),
               ~I(2023-01-02T00:00:00.000000Z → 2023-01-04T23:59:59.999999Z) | _
             ] = Slots.add(slots, ~I|2023-01-02 → 2023-01-04|d, join: false) |> Enum.take(2)
    end

    test "merge/3", %{input: _input, slots: slots} do
      assert [
               ~I(2023-01-01T00:00:00.000000Z → 2023-01-01T23:59:59.999999Z),
               ~I(2023-01-02T00:00:00.000000Z → 2023-01-04T23:59:59.999999Z) | _
             ] =
               Slots.merge(slots, Slots.wrap(~I|2023-01-02 → 2023-01-04|d), join: false)
               |> Enum.take(2)

      assert [
               ~I(2023-01-01T00:00:00.000000Z → 2023-01-05T23:59:59.999999Z),
               ~I(2023-01-07T00:00:00.000000Z → 2023-01-07T23:59:59.999999Z) | _
             ] =
               Slots.merge(slots, Slots.wrap(~I|2023-01-02 → 2023-01-04|d, Slots.Stream),
                 join: true
               )
               |> Enum.take(4)

      assert [
               ~I(2023-01-01T00:00:00.000000Z → 2023-01-05T23:59:59.999999Z),
               ~I(2023-01-07T00:00:00.000000Z → 2023-01-07T23:59:59.999999Z) | _
             ] =
               Slots.merge(slots, Slots.wrap(~I|2023-01-02 → 2023-01-04|d), join: true)
               |> Enum.take(4)
    end
  end
end
