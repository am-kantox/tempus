defmodule Tempus.Slots.Void do
  @moduledoc false

  use Tempus.Telemetria

  alias Tempus.{Slots, Slots.Void}

  defstruct slots: nil

  defimpl Slots.Group do
    def identity(%Void{}), do: %Void{}
    def flatten(%Void{}, _options), do: []
    def add(%Void{}, _slot, _options), do: %Void{}
    def drop_until(%Void{}, _slot, _options), do: {:ok, %Void{}}
    def merge(%Void{}, _other, _options), do: {:error, __MODULE__}
    def inverse(%Void{}, _options), do: {:ok, %Void{}}
  end
end
