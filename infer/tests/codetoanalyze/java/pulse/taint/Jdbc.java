/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

public class Jdbc {

  Connection mConnection;
  Statement mStatement;

  // Building the statement out of a user-controlled string is already an injection, even though
  // the resulting PreparedStatement would be safe if the query were a constant.
  void prepareStatementBad() throws SQLException {
    String user = InferTaint.inferSecretStringSource();
    mConnection.prepareStatement("SELECT * FROM users WHERE name = '" + user + "'"); // report here
  }

  void prepareStatementWithOptionsBad() throws SQLException {
    String user = InferTaint.inferSecretStringSource();
    mConnection.prepareStatement(
        "SELECT * FROM users WHERE name = '" + user + "'",
        ResultSet.TYPE_FORWARD_ONLY,
        ResultSet.CONCUR_READ_ONLY); // report here
  }

  void prepareCallBad() throws SQLException {
    String user = InferTaint.inferSecretStringSource();
    mConnection.prepareCall("{call findUser('" + user + "')}"); // report here
  }

  // Binding the user-controlled value as a parameter of a constant query is the recommended
  // mitigation and must not be reported.
  void bindParameterOk() throws SQLException {
    String user = InferTaint.inferSecretStringSource();
    PreparedStatement statement =
        mConnection.prepareStatement("SELECT * FROM users WHERE name = ?");
    statement.setString(1, user);
    statement.executeQuery();
  }

  void bindCallableParameterOk() throws SQLException {
    String user = InferTaint.inferSecretStringSource();
    CallableStatement statement = mConnection.prepareCall("{call findUser(?)}");
    statement.setString(1, user);
    statement.execute();
  }

  // Same sinks as Service1.paramToSql*Bad in Services.java, reached from a user-controlled string
  // built by concatenation rather than from a Thrift endpoint parameter.
  void concatenatedQueryToExecuteBad() throws SQLException {
    String user = InferTaint.inferSecretStringSource();
    mStatement.execute("DELETE FROM users WHERE name = '" + user + "'"); // report here
  }

  void concatenatedQueryToExecuteQueryBad() throws SQLException {
    String user = InferTaint.inferSecretStringSource();
    mStatement.executeQuery("SELECT * FROM users WHERE name = '" + user + "'"); // report here
  }

  void concatenatedQueryToExecuteUpdateBad() throws SQLException {
    String user = InferTaint.inferSecretStringSource();
    mStatement.executeUpdate("UPDATE users SET name = '" + user + "'"); // report here
  }

  void constantQueryOk() throws SQLException {
    mStatement.executeQuery("SELECT name FROM users");
  }
}
