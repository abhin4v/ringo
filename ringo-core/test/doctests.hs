import Test.DocTest
main = do
  doctest ["-isrc", "Ringo"]
  doctest ["-isrc", "Ringo.Types.Internal"]
