object SpaceAge {
  def secondsToYears(seconds: Double, orbitalPeriod: Double): Double = seconds / 60 / 60 / 24 / 365.25 / orbitalPeriod

  def onEarth(seconds: Double): Double   = { val orbitalPeriod = 1          ; secondsToYears(seconds, orbitalPeriod) }
  def onMercury(seconds: Double): Double = { val orbitalPeriod = 0.2408467  ; secondsToYears(seconds, orbitalPeriod) }
  def onVenus(seconds: Double): Double   = { val orbitalPeriod = 0.61519726 ; secondsToYears(seconds, orbitalPeriod) }
  def onMars(seconds: Double): Double    = { val orbitalPeriod = 1.8808158  ; secondsToYears(seconds, orbitalPeriod) }
  def onJupiter(seconds: Double): Double = { val orbitalPeriod = 11.862615  ; secondsToYears(seconds, orbitalPeriod) }
  def onSaturn(seconds: Double): Double  = { val orbitalPeriod = 29.447498  ; secondsToYears(seconds, orbitalPeriod) }
  def onUranus(seconds: Double): Double  = { val orbitalPeriod = 84.016846  ; secondsToYears(seconds, orbitalPeriod) }
  def onNeptune(seconds: Double): Double = { val orbitalPeriod = 164.79132  ; secondsToYears(seconds, orbitalPeriod) }
}
