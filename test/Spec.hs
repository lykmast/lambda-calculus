import ParseSpec(parse_props)
import TermGenSpec(termGen_props)
import Hedgehog (check)

main :: IO ()
main = mapM_ check $ termGen_props ++ parse_props