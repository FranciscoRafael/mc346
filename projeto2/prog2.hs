---- Nome: Francisco Rafael Capiteli Carneiro   RA: 157888 ---------
---- Nome: Gabriel Capiteli Bertocco            RA: 155419 --------- 




import System.IO
data Vertex = Vertex {v::String,u::String,w::Float} deriving (Show)
data Graph = Graph {nodes::[Vertex]} deriving (Show)
data Distances = Distances {vert::[String], fathers::[String], dists::[Float], seen::[Float]} deriving (Show)
data Path = Path {begin::String, end::String} deriving (Show)


main = do
	let graph = Graph []
	let distances = Distances [] [] [] []
	let path = Path  " " " "
	readGraph graph distances path
	
	


readGraph :: Graph -> Distances -> Path -> IO ()
readGraph graph distances path = do
	arc <- isEOF


	if arc
		then do
			let new_distances = setSource distances (begin path)
			let tree = djikistra graph new_distances path
			let pos_v = foundVertex (vert tree) (end path) 0
			let dist_v = getPosition2 (dists tree) pos_v 0
			let p = createPath tree [] (end path)
			putStrLn $ "inicial: " ++ (begin path)
			putStrLn $ "final: " ++ (end path)
			if(dist_v == 1/0)
				then putStrLn $ "nada"
			else do
				let result_path = invertList p
				putStr $ "custo: "
				print dist_v
				mapM_ (\n -> putStr $ n++" ") result_path
				putStrLn ""

					


	else do inp <- getLine
		let varc = (words inp)

		if((length varc) > 1)
			then do let w = read (last varc) :: Float
				let u = (head varc)
				let v = (head (tail varc))
				let vert = Vertex u v w
				let res = (nodes graph)++[vert]
				let g = Graph res
				let dist = setDistances distances u v
				--print dist
				readGraph g dist path
		else do
			if((begin path) == " ") ---- read source -----
				then do let p = Path (head varc) " "
					readGraph graph	distances p
			else do  ---- read destiny vertex -----
				let p = Path (begin path) (head varc)	
				readGraph graph	distances p


djikistra :: Graph -> Distances -> Path -> Distances
djikistra graph distances path = if ((notEmpty (seen distances)) == 1)
									then do
										let initV = initVertex distances 0
										let aux = extractMin distances (1/0) initV 0
										let updated_distances = neighboors distances (nodes graph) aux
										let fuma_fuma = updateSeen updated_distances aux
										djikistra graph fuma_fuma path
										
								else
									distances		



invertList :: [String] -> [String]
invertList [] = []
invertList (x:xs) = (invertList xs)++[x]

createPath :: Distances -> [String] -> String -> [String]
createPath distances path v = do
								let pos_v = foundVertex (vert distances) v 0
								let father_v = getPosition (fathers distances) pos_v 0
								if(father_v == "nil") then (path++[v])
								else createPath distances (path++[v]) father_v



notEmpty :: [Float] -> Int
notEmpty [] = 0
notEmpty (x:xs) = if(x == 0.0) then 1
					else (notEmpty xs) 

initVertex :: Distances -> Int -> String
initVertex distances i = if((length (vert distances)) == i) then " "
							else
								do
									let vertex = getPosition (vert distances) i 0
									let pos_v = foundVertex (vert distances) vertex 0
									let dist_v = getPosition2 (dists distances) pos_v 0
									if(dist_v == 1/0) then vertex	
									else
										initVertex distances (i+1)			

updateSeen :: Distances -> String -> Distances
updateSeen distances v = do
							let pos = foundVertex (vert distances) v 0
							let dist = getPosition2 (dists distances) pos 0 
							let f = getPosition (fathers distances) pos 0
							let new = Distances [] [] [] []
							update distances dist f 1.0 new pos 0




neighboors :: Distances -> [Vertex] -> String -> Distances 
neighboors distances [] vertex = distances
neighboors distances (x:xs) vertex = if((v x) == vertex)
										then do
											let updated_distances = relax distances vertex (u x) (w x)  
											neighboors updated_distances xs vertex
									else
										neighboors distances xs vertex		




relax :: Distances -> String -> String -> Float -> Distances
relax distances u v w = do 
							let pos_v = foundVertex (vert distances) v 0
							let pos_u = foundVertex (vert distances) u 0
							let dist_v = getPosition2 (dists distances) pos_v 0
							let dist_u = getPosition2 (dists distances) pos_u 0
							if(dist_v > dist_u + w)
								then do
									let new_dist_v = dist_u + w
									let new = Distances [] [] [] []
									let s = getPosition2 (seen distances) pos_v 0
									update distances new_dist_v u s new pos_v 0
							else
								distances		


---- extract the vertex with minimum distances ---- 
extractMin :: Distances -> Float -> String -> Int -> String
extractMin distances d v i = if (length (vert distances) == i)
								then v
							else do
								
								let dist_vert = getPosition2 (dists distances) i 0
								let vertex = getPosition (vert distances) i 0
								let inDist = getPosition2 (seen distances) i 0
								if(inDist == 0.0) 
									then
										if (dist_vert < d)
											then extractMin distances dist_vert vertex (i+1)
										else
											extractMin distances d v (i+1)
								else
									extractMin distances d v (i+1)				



--- get the ith position of the vector ---- 
getPosition :: [String] -> Int -> Int -> String
getPosition (x:xs) pos i = if(pos == i) then x
							else getPosition xs pos (i+1)

--- get the ith position of the vector ---- 
getPosition2 :: [Float] -> Int -> Int -> Float
getPosition2 (x:xs) pos i = if(pos == i) then x
							else getPosition2 xs pos (i+1)							


----- Functions to set zero in the distance source --------
foundVertex :: [String] -> String -> Int -> Int
foundVertex [] v acc = 0
foundVertex (x:xs) v acc = if(x == v) then acc
							else foundVertex xs v (acc+1)


setSource :: Distances -> String -> Distances
setSource distances s = do 
							let pos = foundVertex (vert distances) s 0
							let new = Distances [] [] [] []
 							update distances 0.0 "nil" 0.0 new pos 0


update :: Distances -> Float -> String -> Float -> Distances -> Int -> Int -> Distances
update distances dist father s acc_distances pos acc = if((length (vert distances)) == acc)
															then acc_distances
														else if(pos == acc) 
															then do 
																let x = getPosition (vert distances) acc 0
																let new = Distances ((vert acc_distances)++[x]) ((fathers acc_distances)++[father]) ((dists acc_distances)++[dist]) ((seen acc_distances)++[s])
																update distances dist father s new pos (acc+1)
														else do
															let v = getPosition2 (dists distances) acc 0 
															let x = getPosition (vert distances) acc 0
															let s_new = getPosition2 (seen distances) acc 0
															let f = getPosition (fathers distances) acc 0
															let new = Distances ((vert acc_distances)++[x]) ((fathers acc_distances)++[f]) ((dists acc_distances)++[v]) ((seen acc_distances)++[s_new])
															update distances dist father s new pos (acc+1)



------ Set "infinite" initially in the vertices distances from the source ---- 			
setDistances :: Distances -> String -> String -> Distances
setDistances distances u v = if((verify (vert distances) u) == 0)
								then do let dist = Distances ((vert distances)++[u]) ((fathers distances)++["nil"]) ((dists distances)++[1/0]) ((seen distances)++[0.0])
									if((verify (vert dist) v) == 0)
										then do let dist2 = Distances ((vert dist)++[v]) ((fathers dist)++["nil"]) ((dists dist)++[1/0]) ((seen dist)++[0.0])
											dist2
									else
										dist
							else if((verify (vert distances) v) == 0)
									then do let dist = Distances ((vert distances)++[v]) ((fathers distances)++["nil"]) ((dists distances)++[1/0]) ((seen distances)++[0.0])
							 			dist
							else 	
								distances
								
							 	
verify :: [String] -> String -> Integer
verify [] a = 0	
verify (x:xs) a = if(x == a) then 1
				else verify xs a


		  


				




		  



