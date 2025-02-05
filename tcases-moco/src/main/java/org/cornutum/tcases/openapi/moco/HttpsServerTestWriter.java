//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import org.cornutum.tcases.io.IndentedWriter;
import org.cornutum.tcases.openapi.testwriter.ApiTestWriter;
import org.cornutum.tcases.openapi.testwriter.TestCaseWriter;

/**
 * A JUnitTestWriter for API tests that use a <a href="https://github.com/dreamhead/moco/blob/master/moco-doc/junit.md#https-server">Moco HttpsServer</a>
 */
@ApiTestWriter( name="moco-https", target="java")
public class HttpsServerTestWriter extends MocoServerTestWriter
  {
  /**
   * Creates a new HttpsServerTestWriter instance.
   */
  public HttpsServerTestWriter( MocoServerConfig serverConfig, CertConfig certConfig, TestCaseWriter testCaseWriter)
    {
    super( serverConfig, certConfig, testCaseWriter);
    }

  /**
   * Returns the Moco server class name for this test writer.
   */
  @Override
  protected String getServerClass()
    {
    return "HttpsServer";
    }

  /**
   * Returns the Moco server factory method for this test writer.
   */
  @Override
  protected String getServerFactory()
    {
    return "httpsServer";
    }

  /**
   * Returns the MocoJunitRunner factory method for this test writer.
   */
  @Override
  protected String getRunnerFactory()
    {
    return "httpsRunner";
    } 

  /**
   * Writes the target test dependencies for the POJO server configuration to the given stream.
   */
  @Override
  protected void writePojoDependencies( IndentedWriter targetWriter)
    {
    targetWriter.println( "import com.github.dreamhead.moco.HttpsServer;");
    }
  }
