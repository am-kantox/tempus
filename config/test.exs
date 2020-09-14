import Config

config :telemetria,
  enabled: true,
  events: [
    [:tempus, :add],
    [:tempus, :next_busy],
    [:tempus, :next_free],
    [:tempus, :slots, :add],
    [:tempus, :slots, :inverse],
    [:tempus, :slots, :merge]
  ]
