//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import org.cornutum.tcases.io.IndentedWriter;
import org.cornutum.tcases.openapi.testwriter.TestCaseWriter;

/**
 * A JUnitTestWriter for API tests that use a <a href="https://github.com/dreamhead/moco/blob/master/moco-doc/junit.md#http-server">Moco HttpServer</a>
 */
public class HttpServerTestWriter extends MocoServerTestWriter
  {
  /**
   * Creates a new HttpServerTestWriter instance.
   */
  public HttpServerTestWriter( MocoServerConfig serverConfig, TestCaseWriter testCaseWriter)
    {
    super( serverConfig, null, testCaseWriter);
    }

  /**
   * Returns the Moco server class name for this test writer.
   */
  @Override
  protected String getServerClass()
    {
    return "HttpServer";
    }

  /**
   * Returns the Moco server factory method for this test writer.
   */
  @Override
  protected String getServerFactory()
    {
    return "httpServer";
    }

  /**
   * Returns the MocoJunitRunner factory method for this test writer.
   */
  @Override
  protected String getRunnerFactory()
    {
    return "httpRunner";
    }    

  /**
   * Writes the target test dependencies for the POJO server configuration to the given stream.
   */
  @Override
  protected void writePojoDependencies( IndentedWriter targetWriter)
    {
    targetWriter.println( "import com.github.dreamhead.moco.HttpServer;");
    }
  }
