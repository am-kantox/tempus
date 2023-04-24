defprotocol Tempus.Slots.Group do
  @moduledoc """
  The protocol to implement for the ordered collection of slots.
  """

  alias Tempus.{Slot, Slots}

  @doc """
  The function returning the identity element for this `t:Slots.container()`
    implementation.
  """
  @spec identity(Slots.container()) :: Slots.container()
  def identity(slots)

  @doc """
  Flattens the implementation of `t:Slots.container()`, returning the list back.
  """
  @spec flatten(Slots.container(), keyword()) :: [Slot.t()]
  def flatten(slots, options)

  @doc """
  Adds a single `t:Slot.t()` instance to this `t:Slots.container()`
    implementation. If `merge/3` implementation returns `{:error, __MODULE__}`,
    this function would be used to add elements one by one through `reduce/3`.
  """
  @spec add(Slots.container(), Slot.origin(), keyword()) :: Slots.container()
  def add(slots, slot, options)

  @doc """
  Efficient implementation of fast forwarding slots. If this function
    returns `{:error, __MODULE__}`, the `reduce/3` will be used instead.
  """
  @spec drop_until(Slots.container(), Slot.origin(), keyword()) ::
          {:ok, Slots.container()} | {:error, module()}
  def drop_until(slots, origin, options)

  @doc """
  Efficient implementation of merging slots. If this function
    returns `{:error, __MODULE__}`, the `reduce/3` will be used instead.
  """
  @spec merge(Slots.container(), [Slot.t()] | Slots.container(), keyword()) ::
          {:ok, Slots.container()} | {:error, module()}
  def merge(slots, other, options)

  @doc """
  Efficient implementation of inversing slots. If this function
    returns `{:error, __MODULE__}`, the `reduce/3` will be used instead.
  """
  @spec inverse(Slots.container(), keyword()) :: {:ok, Slots.container()} | {:error, module()}
  def inverse(slots, options)
end
