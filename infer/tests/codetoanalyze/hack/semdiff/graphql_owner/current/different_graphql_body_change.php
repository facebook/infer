<?hh

<<Oncalls('my_oncall')>>
final class MyGraphQLQuery {
  const GRAPHQL_ARTIFACT_TEXT = 'query MyQuery($id: ID!)
@owner(oncall: "my_oncall") {
  node(id: $id) {
    name
  }
}';

  public function process(): string {
    return "goodbye";
  }
}
