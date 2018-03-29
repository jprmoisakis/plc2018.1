-- Retornar todas as sublistas de uma lista
-- sublista[1,2,3] = [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]]

sublista :: [a] -> [[a]] -- Não entendo
sublista [] = [[]]
sublista (x : xs) = [x : xs | xs <- sublista xs] ++ sublista xs