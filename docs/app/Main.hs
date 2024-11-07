import CircularBuffer.Model (prop_CircularBuffer)
import Test.QuickCheck (quickCheck)

main :: IO ()
main = do
  quickCheck prop_CircularBuffer
