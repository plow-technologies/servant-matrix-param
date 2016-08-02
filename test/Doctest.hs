
import Test.DocTest

main :: IO ()
main = doctest $
  "src/Servant/MatrixParam.hs" :
  "-isrc" :
  "-XDataKinds" :
  "-XTypeOperators" :
  []
