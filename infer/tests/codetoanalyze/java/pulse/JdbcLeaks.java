/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Properties;

// Connections returned by DriverManager.getConnection (issue #2111)
public class JdbcLeaks {

  public void getConnectionNotClosedBad() throws SQLException {
    DriverManager.getConnection("jdbc:test");
  }

  public void getConnectionNotClosedAfterCommitBad(String user, String password)
      throws SQLException {
    Connection connection = DriverManager.getConnection("jdbc:test", user, password);
    connection.setAutoCommit(false);
    connection.commit();
  }

  public void getConnectionClosedOk(Properties info) throws SQLException {
    Connection connection = DriverManager.getConnection("jdbc:test", info);
    try {
      connection.setAutoCommit(false);
      connection.commit();
    } finally {
      connection.close();
    }
  }

  public void getConnectionTryWithResourcesOk(String sql) throws SQLException {
    try (Connection connection = DriverManager.getConnection("jdbc:test");
        PreparedStatement statement = connection.prepareStatement(sql)) {
      statement.execute();
    }
  }

  public Connection getConnectionReturnedOk() throws SQLException {
    return DriverManager.getConnection("jdbc:test");
  }
}
