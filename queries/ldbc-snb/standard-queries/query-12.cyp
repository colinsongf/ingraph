MATCH (:Person)-[:KNOWS]-(friend:Person)
OPTIONAL MATCH (friend)<-[:HAS_CREATOR]-(comment:Comment)-[:REPLY_OF]->(:Post)-[:HAS_TAG]->(tag:Tag),
(tag)-[:HAS_TYPE]->(tagClass:TagClass)-[:IS_SUBCLASS_OF*0..]->(baseTagClass:TagClass)
WHERE tagClass.name = $class
   OR baseTagClass.name = $class
RETURN
  friend.id AS friendId,
  friend.firstName AS friendFirstName,
  friend.lastName AS friendLastName,
  collect(DISTINCT tag.name) AS tagNames,
  count(DISTINCT comment) AS count
ORDER BY
  count DESC,
  friendId ASC
LIMIT 10
