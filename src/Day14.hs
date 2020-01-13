module Day14 where

import Data.List
import Data.List.Split
import Debug.Trace

type Chemical = String
type ChemicalAmount = (Int, String)
type Reaction = ([ChemicalAmount], ChemicalAmount)

input1s = ["9 RJLWC, 9 RJCH => 9 QWFH","1 XZVHQ, 9 SPQR, 2 WKGVW => 5 KPZB","12 HPRPM, 4 GTZCK => 7 DJNDX","7 JKRV, 3 FKTLR, 19 FDSBZ => 9 HPRPM","9 VTCRJ => 4 SPSW","2 FDSBZ, 1 FKTLR => 6 KBJF","9 SPSW => 9 QHVSJ","5 TFPNF, 11 MNMBX, 1 QCMJ, 13 TXPL, 1 DJNDX, 9 XZVHQ, 2 WKGVW, 2 VQPX => 8 GPKR","10 DWTC, 8 DSPJG => 4 QCMJ","100 ORE => 9 XZDP","3 DBRBD => 4 DKRX","37 JKRV, 5 FKTLR => 7 VXZN","3 HWDS, 2 ZRBN => 8 XZVHQ","15 QNXZV, 53 VXZN, 3 LJQH, 13 FKXVQ, 6 DZGN, 17 MNMBX, 16 GPKR, 8 HWJVK => 1 FUEL","8 GSLWP => 7 PWTFL","4 HVPWG => 9 JKRV","5 NVWGS, 1 QWFH, 9 CWZRS => 2 XPMV","6 ZRBN => 4 JZDB","36 BWXWC, 14 HKFD => 3 FMNK","3 FMNK, 2 SPSW, 16 WKGVW => 6 VQPX","1 DWTC => 9 VMHM","3 HPRPM, 1 DWTC => 5 TXPL","1 KBJF, 2 ZSKSW => 1 MNMBX","5 JZDB => 4 FDSBZ","2 FKXVQ => 9 ZTFZG","17 XZDP => 2 HKFD","7 VMHM => 3 FGQF","1 JKRV => 8 CWZRS","1 WKGVW, 2 SPSW => 6 VLQP","3 ZRBN => 3 ZSKSW","7 VXZN, 7 TGLHX => 5 NVWGS","10 VLQP, 18 FGQF => 4 DBRBD","8 VMHM => 8 SPQR","1 KPZB, 4 GQGB, 3 WKGVW => 1 FDSZX","2 VXZN => 8 VTCRJ","3 RJLWC => 2 GQGB","6 TXPL => 4 DSPJG","2 ZTFZG => 8 TJLW","1 MPSPS => 3 BWXWC","5 FMNK, 4 ZSKSW => 5 RWKWD","137 ORE => 3 MPSPS","1 VTCRJ, 8 QWFH => 2 GKVQK","8 RJLWC => 8 TFPNF","7 TJLW, 1 TFPNF, 16 VQPX, 4 DBRBD, 4 GTZCK, 5 XPMV, 1 FDSZX => 6 DZGN","1 HVPWG => 7 RJLWC","18 HVPWG, 9 BWXWC => 4 GSLWP","107 ORE => 8 RJCH","1 RJCH => 2 ZRBN","2 GSLWP, 18 RWKWD, 1 QWFH => 5 LJQH","3 VXZN, 1 FMNK => 4 TGLHX","3 HKFD, 6 FMNK => 3 FKTLR","3 MPSPS => 4 HVPWG","27 PWTFL, 15 ZTFZG, 6 QHVSJ, 14 DJNDX, 9 RWKWD, 2 MNMBX, 4 DKRX => 6 QNXZV","1 ZSKSW, 9 KBJF => 3 FKXVQ","2 FDSBZ => 4 DWTC","3 HPRPM => 5 HWDS","1 GKVQK, 1 PWTFL => 5 GTZCK","1 FGQF => 5 WKGVW","5 FDSBZ, 7 SPSW => 6 HWJVK"]
input1 = map parseInput input1s
example1 = map parseInput ["10 ORE => 10 A","1 ORE => 1 B","7 A, 1 B => 1 C","7 A, 1 C => 1 D","7 A, 1 D => 1 E","7 A, 1 E => 1 FUEL"]
example2 = map parseInput ["9 ORE => 2 A","8 ORE => 3 B","7 ORE => 5 C","3 A, 4 B => 1 AB","5 B, 7 C => 1 BC","4 C, 1 A => 1 CA","2 AB, 3 BC, 4 CA => 1 FUEL"]
example3 = map parseInput ["157 ORE => 5 NZVS","165 ORE => 6 DCFZ","44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL","12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ","179 ORE => 7 PSHF","177 ORE => 5 HKGWZ","7 DCFZ, 7 PSHF => 2 XJWVT","165 ORE => 2 GPVTF","3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"]
example4 = map parseInput ["2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG","17 NVRVD, 3 JNWZP => 8 VPVL","53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL","22 VJHF, 37 MNCFX => 5 FWMGM","139 ORE => 4 NVRVD","144 ORE => 7 JNWZP","5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC","5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV","145 ORE => 6 MNCFX","1 NVRVD => 8 CXFTF","1 VJHF, 6 MNCFX => 4 RFSQX","176 ORE => 6 VJHF"]
example5 = map parseInput ["171 ORE => 8 CNZTR","7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL","114 ORE => 4 BHXH","14 VRPVC => 6 BMBT","6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL","6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT","15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW","13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW","5 BMBT => 4 WPTQ","189 ORE => 9 KTJDG","1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP","12 VRPVC, 27 CNZTR => 2 XDBXC","15 KTJDG, 12 BHXH => 5 XCVML","3 BHXH, 2 VRPVC => 7 MZWV","121 ORE => 7 VRPVC","7 XCVML => 6 RJRHP","5 BHXH, 4 VRPVC => 5 LTCX"]


parseInput :: String -> Reaction
parseInput s = (lefts, parseAmount right)
  where
    [left, right] = splitOn " => " s
    lefts = map parseAmount $ splitOn ", " left
    parseAmount s = (read as :: Int, cs)
      where [as, cs] = splitOn " " s

desynth :: [ChemicalAmount] -> [Reaction] -> [ChemicalAmount]
desynth amounts reactions = case (find (\(n, chem) -> chem /= "ORE" && n > 0) amounts) of
  Nothing -> amounts
  Just (n, chem) -> desynth newAmounts reactions
    where
      newAmounts = desynthReaction reaction times
      times = (div n nout) + if mod n nout == 0 then 0 else 1
      reaction = findReaction chem reactions
      desynthReaction reaction times = desynthOne (multiplyReaction times) amounts
      (rin, (nout, _)) = reaction
      multiplyReaction t = (map (\(nin, cin) -> (nin * t, cin)) rin, (nout * t, chem))


findReaction :: Chemical -> [Reaction] -> Reaction
findReaction c reactions = case (find (\(_, (_, cc)) -> c == cc) reactions) of
  Nothing -> error "cannot find reaction"
  Just r -> r

desynthOne :: Reaction -> [ChemicalAmount] -> [ChemicalAmount]
desynthOne (froms, to) amounts = subtractChem (foldl addChem amounts froms) to

addChem :: [ChemicalAmount] -> ChemicalAmount -> [ChemicalAmount]
addChem [] a = [a]
addChem ((n, c) : rest) (an, ac)
  | c == ac = (n + an, c) : rest
  | otherwise = (n, c) : addChem rest (an, ac)

subtractChem :: [ChemicalAmount] -> ChemicalAmount -> [ChemicalAmount]
subtractChem [] (n, c) = [(-n, c)]
subtractChem ((n, c) : rest) (an, ac)
  | c /= ac = (n, c) : subtractChem rest (an, ac)
  | n == an = rest
  | otherwise = (n - an, c) : rest

makeFuel :: [Reaction] -> Int
makeFuel reactions = makeMoreFuel reactions 1

makeMoreFuel :: [Reaction] -> Int -> Int
makeMoreFuel reactions f = case (find (\(_, c) -> c == "ORE") (desynth [(f, "FUEL")] reactions)) of
  Just (n, _) -> n
  Nothing -> error "should contain ORE"

answer1 = makeFuel input1

trillion = 1000000000000

trillionOre :: [Reaction] -> Int
trillionOre rs = torAdd $ div trillion $ makeFuel rs
  where
    torAdd fuel
      | (makeMoreFuel rs fuel) > trillion = torDel fuel
      | otherwise = torAdd $ fuel + 1000
    torDel fuel
      | (makeMoreFuel rs fuel) <= trillion = fuel
      | otherwise = torDel $ fuel - 1

answer2 = trillionOre input1
