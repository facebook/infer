<?hh

<<Oncalls('my_oncall')>>
final class MyGraphQLQuery {
  const GRAPHQL_ARTIFACT_TEXT = 'query MyQuery($id: ID!) {
  node(id: $id) {
    name
  }
}';
}
