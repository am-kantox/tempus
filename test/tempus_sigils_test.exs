defmodule Tempus.Sigils.Test do
  use ExUnit.Case, async: false
  doctest Tempus.Sigils

  alias Tempus.Slot
  import Tempus.Sigils
  import ExUnit.CaptureIO

  test "sigils" do
    assert ~I[2021-03-30]d == %Slot{
             from: ~U[2021-03-30 00:00:00.000000Z],
             to: ~U[2021-03-30 23:59:59.999999Z]
           }

    assert ~I[07:00:00]t == %Slot{
             from: %DateTime{
               DateTime.utc_now()
               | hour: 7,
                 minute: 0,
                 second: 0,
                 microsecond: {0, 0}
             },
             to: %DateTime{
               DateTime.utc_now()
               | hour: 7,
                 minute: 0,
                 second: 0,
                 microsecond: {0, 0}
             }
           }
  end

  test "fancy inspect" do
    assert capture_io(fn ->
             # credo:disable-for-next-line
             IO.inspect(~I[2021-03-30]d, custom_options: [fancy: true])
           end) == "𝕥(2021-03-30T00:00:00.000000Z → 2021-03-30T23:59:59.999999Z)\n"

    assert capture_io(fn ->
             # credo:disable-for-next-line
             IO.inspect(~I[2021-03-30]d, custom_options: [fancy: :emoji])
           end) == "⌚(2021-03-30T00:00:00.000000Z → 2021-03-30T23:59:59.999999Z)\n"

    slots =
      Enum.into(
        [~D|2020-08-06|, ~D|2020-08-08|, ~D|2020-08-10|, ~D|2020-08-12|, ~D|2020-08-14|],
        %Tempus.Slots{}
      )

    assert capture_io(fn ->
             # credo:disable-for-next-line
             IO.inspect(slots, custom_options: [truncate: true, fancy: :emoji])
           end) ==
             "#𝕋<𝕥ˡ<[⌚(2020-08-06T00:00:00.000000Z → 2020-08-06T23:59:59.999999Z),\n \"… ‹3 more› …\",\n ⌚(2020-08-14T00:00:00.000000Z → 2020-08-14T23:59:59.999999Z)]>>\n"

    assert capture_io(fn ->
             # credo:disable-for-next-line
             IO.inspect(slots, custom_options: [truncate: 1, fancy: :emoji])
           end) ==
             "#𝕋<𝕥ˡ<[⌚(2020-08-06T00:00:00.000000Z → 2020-08-06T23:59:59.999999Z),\n \"… ‹2 more› …\",\n ⌚(2020-08-12T00:00:00.000000Z → 2020-08-12T23:59:59.999999Z),\n ⌚(2020-08-14T00:00:00.000000Z → 2020-08-14T23:59:59.999999Z)]>>\n"
  end
end
