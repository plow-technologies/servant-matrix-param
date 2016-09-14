
import Test.DocTest

main :: IO ()
main = doctest $
  "src/Servant/MatrixParam.hs" :
  "src/Servant/MatrixParam/Internal.hs" :
  "-isrc" :
  "-XDataKinds" :
  "-XTypeOperators" :
  []
