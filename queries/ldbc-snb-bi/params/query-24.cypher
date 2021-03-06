MATCH
  (:TagClass)<-[:hasType]-(:Tag)<-[:hasTag]-(message:Message)<-[:likes]-(person:Person),
  (message)-[:is_LOCATED]->(:Country)-[:isPartOf]->(continent:Continent)
RETURN count(message) AS messageCount, count(person) AS likeCount, message.creationDate AS year, message.creationDate AS month, continent.name
