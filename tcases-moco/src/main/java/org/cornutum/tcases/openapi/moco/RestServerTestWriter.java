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
 * A JUnitTestWriter for API tests that use a <a href="https://github.com/dreamhead/moco/blob/master/moco-doc/junit.md#rest-server">Moco RestServer</a>
 */
public class RestServerTestWriter extends MocoServerTestWriter
  {
  /**
   * Creates a new RestServerTestWriter instance.
   */
  public RestServerTestWriter( MocoServerConfig serverConfig, TestCaseWriter testCaseWriter)
    {
    super( serverConfig, testCaseWriter);
    }

  /**
   * Returns the Moco server class name for this test writer.
   */
  protected String getServerClass()
    {
    return "RestServer";
    }

  /**
   * Returns the Moco server factory method for this test writer.
   */
  protected String getServerFactory()
    {
    return "restServer";
    }

  /**
   * Returns the MocoJunitRunner factory method for this test writer.
   */
  protected String getRunnerFactory()
    {
    return "restRunner";
    }    

  /**
   * Writes the target test dependencies for the POJO server configuration to the given stream.
   */
  protected void writePojoDependencies( IndentedWriter targetWriter)
    {
    targetWriter.println( "import com.github.dreamhead.moco.RestServer;");
    targetWriter.println( "import static com.github.dreamhead.moco.MocoRest.*;");
    }
  }
