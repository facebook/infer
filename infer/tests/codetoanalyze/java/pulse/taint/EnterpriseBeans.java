/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.io.IOException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import javax.ejb.Remote;
import javax.ejb.Stateless;

// The methods of a remote EJB interface can be called by any client of the application server, so
// their parameters are controlled by the caller.

class EnterpriseBeans {}

@Remote
interface AccountServiceRemote {

  ResultSet findAccountBad(String owner) throws SQLException;

  void exportAccountBad(AccountExport export) throws IOException;

  int countAccountsOk() throws SQLException;
}

class AccountExport {
  String owner;
  String format;
}

@Stateless
class AccountServiceBean implements AccountServiceRemote {

  Statement statement;

  @Override
  public ResultSet findAccountBad(String owner) throws SQLException {
    return statement.executeQuery("SELECT * FROM accounts WHERE owner = '" + owner + "'");
  }

  @Override
  public void exportAccountBad(AccountExport export) throws IOException {
    Runtime.getRuntime().exec("/usr/bin/export-account --format " + export.format);
  }

  @Override
  public int countAccountsOk() throws SQLException {
    ResultSet result = statement.executeQuery("SELECT COUNT(*) FROM accounts");
    return result.next() ? result.getInt(1) : 0;
  }

  // not part of the remote interface: only called from inside the application
  public ResultSet findAccountNotRemoteOk(String owner) throws SQLException {
    return statement.executeQuery("SELECT * FROM accounts WHERE owner = '" + owner + "'");
  }
}

interface ReportService {

  void generateReportBad(String name) throws IOException;
}

// the remote interface can also be declared on the bean class
@Stateless
@Remote(ReportService.class)
class ReportServiceBean implements ReportService {

  @Override
  public void generateReportBad(String name) throws IOException {
    Runtime.getRuntime().exec("/usr/bin/generate-report " + name);
  }
}
