(*
本註解屬於程式碼一部分
Under MIT License
(c) 2023 Tan, Kian-ting

==========8964路路线资讯==================================
起讫站：程式码→墙内
票价：Free（<五毛人民币）
時刻：机动发车，单向行驶
停靠站：程式码→民主化→六四天安门→自由门下载→毋忘六四→刘晓波→
台湾独立→民运→西藏独立→新疆独立→港独→九评共产党→法轮功→
Tiananmen Massacre→Free Tibet→ 占领中环→民主→真普选→
南方街头运动→新公民运动→东突厥斯坦→湖南共和国→上访→ 大纪元→胡耀邦
→赵紫阳→Tank Man→北京之春→达赖喇嘛→六四真相→无界下载→通商宽衣→
躺平就是正义→习包子→梁家河小学博士→清零宗→习炀帝→庆丰大帝→
独裁国贼→新疆集中营→光复香港时代革命→祈翠→南蒙古独立→香港独立→
Free Hong Kong→天安门屠杀→中国言论钳制→中共文字狱→
如何润到墙外→中国青年失业率真相→历史的伤口→白纸革命→四通桥事件→
墙内*)

(*我們先假設 P (i,j)，意思是當裡面有 i 行文，j 張圖的時候，存放的分頁資訊，其型別為 Pagination*)
type pagination = {mutable isPagizable : bool; mutable noOfPages : int; mutable predecessor : int * int};;



let t = 300;; (* 行數 *)
let f = 25;; (* 圖數 *)

let maxHeight = 27.0;;
let minHeight = 20.0;;

(*行屬性：height 高度，spaceHeight 其後空白高度，isPagizable 其後空白是否可分頁*)
type line = {height : float; spaceHeight : float; mutable isPagizable : bool};;

let tArray = Array.make (t+1) {height = 0.8; spaceHeight = 0.7; isPagizable = true};;

let fArray = Array.make (f+1) {height = 12.0; spaceHeight = 1.1; isPagizable = true};;

(*tArray與fArray 第0行是空行*)
let _ = tArray.(0) <- {height = 0.0; spaceHeight = 0.0; isPagizable = false} in
let _ = fArray.(0) <- {height = 0.0; spaceHeight = 0.0; isPagizable = false} in
for i = 1 to t do
  if i / 9 == 3 then
    let _ = tArray.(i).isPagizable <- false in ()
  else
  ()
done

(*連 p 0 0 都要加進去所以需要輸入 t+1 和 f+1 的矩陣*)
let p = Array.init (t+1) (fun x -> (Array.init (f+1) (fun x -> {isPagizable = false; noOfPages = -1; predecessor=(-1,-1)})) );;

let nil = -1;; (*condition value*)


(* 圖片 k 對到哪一個文字*)
let referId k = 
if k <= 1 then 1
else if k <= 3 then 9
else if k <= 6 then 40
else if k==7 then 42
else if k <= 10 then 70
else if k == 11 then 80
else if k <= 15 then 112
else if k == 16 then 120
else if k == 17 then 130
else if k <= 22 then 179
else 180

(*組頁*)
let makeOnePage (p : pagination array array) a b i j = 
  if a == i && b == j then false
  else if p.(a).(b).isPagizable == false then false
  else if (referId j) > i || (referId j) == 0 then false
  else 
    let textAndSpaceHeight = 
    if a == i then 0.0
    else
      let textHeight = Array.fold_right (+.) (Array.map (fun x -> x.height) (Array.sub tArray (a+1) (i-a))) 0.0 in
      let textSpaceHeight = Array.fold_right (+.)  (Array.map (fun x -> x.spaceHeight) (Array.sub tArray (a+1) (i-a-1))) 0.0 in
      textHeight +. textSpaceHeight in
    let figAndSpaceHeight = 
    if b == j then 0.0
    else
      let figHeight = Array.fold_right (+.)  (Array.map (fun x -> x.height) (Array.sub fArray (b+1) (j-b))) 0.0  in
      let figSpaceHeight = Array.fold_right (+.)  (Array.map (fun x -> x.spaceHeight) (Array.sub fArray (b+1) (j-b-1))) 0.0 in
      figHeight +. figSpaceHeight in
  let inputHeight = textAndSpaceHeight +. figAndSpaceHeight in
  if inputHeight > maxHeight then false
  else if inputHeight < minHeight then false
  else true;;


let tracePageLs p i j pred = 
  let tracePage = ref [(i, j)] in
  let movablePred = ref pred in

  let _ =
     while not (!movablePred = (-1, -1)) do
     let _ = tracePage := !movablePred :: !tracePage in
     let (x, y) = !movablePred in
     movablePred := p.(x).(y).predecessor
     done
  in (!tracePage);;

let tracePageLen tracePageList = (List.length tracePageList) -1 ;;

let tracePageLsStringfied tracePageList = List.fold_right (fun x y -> x ^", "^ y)
                              (List.map (fun x -> let (t, f) = x in
                                Printf.sprintf "(%d, %d)" t f) tracePageList) "";;


let pageNoOfFigs tracePageList =
  let tracePageListFigPart = List.map (fun x -> let (t, f) = x in f) tracePageList in
  let result = ref [] in
  let _ = for  i = 1 to (List.length tracePageListFigPart) - 1 do
    let diff = (List.nth tracePageListFigPart i)- (List.nth tracePageListFigPart (i-1)) in
    result := List.append !result (List.init diff (fun x -> i))
  done in
  !result ;;

let rec lineNoToPageAux lineNo tracePageListLinePart res = 
  if tracePageListLinePart == [] then 0
  else if List.hd tracePageListLinePart > lineNo then res
  else lineNoToPageAux lineNo (List.tl tracePageListLinePart) (res + 1);;

let lineNoToPage lineNo tracePageLs = lineNoToPageAux lineNo tracePageLs 0;;

let figsToReferPage maxFigNo tracePageLs = 
  let figArray = List.tl (List.init (maxFigNo +1) (fun d -> d)) in
  let referArray = List.map (fun x -> referId x) figArray in
  let tracePageListLinePart = List.map (fun x -> let (t, f) = x in t) tracePageLs in
  let pageOfRefers = List.map (fun x -> lineNoToPage x tracePageListLinePart) referArray in

  pageOfRefers;; 


let diffSumOfRefererAndFig tracePageList f = 
  let pageNoOfFigsList = pageNoOfFigs tracePageList in
  let pageNoOfRefersList = figsToReferPage f tracePageList in
  let combined = List.combine pageNoOfFigsList pageNoOfRefersList in
  let diffOfCombined = List.map (fun x -> let (y,z) = x in y - z) combined in
  let diffSum = List.fold_right (+) diffOfCombined 0 in 
  diffSum;;



let totalCost p t f = 
  let tracePageExample = tracePageLs p t f p.(t).(f).predecessor in
  let diffSum = diffSumOfRefererAndFig tracePageExample f in
  let pageLen = tracePageLen tracePageExample in

  let costFunction pageLen diffSum =
    (let a = 1 in (*weight factor a for pageLen*)
    let b = 1 in (*weight factor b for diffSum*)
    let totalCost = a * pageLen + b * diffSum in
  totalCost)  in

  let totalCost = costFunction pageLen diffSum in
totalCost;;


let chooseBetterPred p oldAB newAB = 
  let (oldA, oldB) = oldAB in
  let (newA, newB) = newAB in
  if oldA == -1 && oldB == -1 then newAB
  else 
    let costOld = totalCost p oldA oldB in
    let costNew = totalCost p newA newB in
    if costNew < costOld then newAB
    else oldAB;;

(*以下是分頁程式*)
let pagize p = 
let _ = p.(0).(0) <- {isPagizable = true; noOfPages = 0;  predecessor = (nil , nil)} in
for i=0 to t do
for j=0 to f do
  for a = 0 to i do
  for b = 0 to j do
      if i != 0 || j != 0 then
      let canMakeOnePage =  makeOnePage p a b i j in
      if canMakeOnePage then
      let (oldA, oldB) = p.(i).(j).predecessor in
      let (preferedA, preferedB) = chooseBetterPred p (oldA, oldB)  (a,b) in

      let _ = p.(i).(j).isPagizable <- true in
      let _ = p.(i).(j).predecessor <- (preferedA, preferedB) in
      let _ = p.(i).(j).noOfPages <- p.(i).(j).noOfPages + 1 in
      ()
      else
      ()
  done
  done
done
done;;


pagize p;;

let x = tracePageLs p 300 25 p.(300).(25).predecessor;;

print_string (tracePageLsStringfied x);;