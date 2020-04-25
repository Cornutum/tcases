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
 * A JUnitTestWriter for API tests that use a <a href="https://github.com/dreamhead/moco/blob/master/moco-doc/junit.md#socket-server">Moco SocketServer</a>
 */
public class SocketServerTestWriter extends MocoServerTestWriter
  {
  /**
   * Creates a new SocketServerTestWriter instance.
   */
  public SocketServerTestWriter( MocoServerConfig serverConfig, TestCaseWriter testCaseWriter)
    {
    super( serverConfig, null, testCaseWriter);
    }

  /**
   * Returns the Moco server class name for this test writer.
   */
  protected String getServerClass()
    {
    return "SocketServer";
    }

  /**
   * Returns the Moco server factory method for this test writer.
   */
  protected String getServerFactory()
    {
    return "socketServer";
    }

  /**
   * Returns the MocoJunitRunner factory method for this test writer.
   */
  protected String getRunnerFactory()
    {
    return "socketRunner";
    }    

  /**
   * Writes the target test dependencies for the POJO server configuration to the given stream.
   */
  protected void writePojoDependencies( IndentedWriter targetWriter)
    {
    targetWriter.println( "import com.github.dreamhead.moco.SocketServer;");
    }
  }
