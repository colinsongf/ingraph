MATCH (a)--(b)--(c)--(d)--(a), (b)--(d)
WHERE a.id = 1
AND c.id = 2
RETURN d
