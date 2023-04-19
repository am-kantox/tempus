defmodule Tempus.Slots.Behaviour do
  @moduledoc """
  The bunch of functions expected to be implemented by `Tempus.Slots` implementations.
  """

  @doc """
  The callback to be implemented by implementations to create new instances
  """
  @callback new :: %{
              __struct__: module(),
              slots: Tempus.Slots.container()
            }
end
