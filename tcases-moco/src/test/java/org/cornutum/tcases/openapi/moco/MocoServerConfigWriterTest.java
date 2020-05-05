//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import org.junit.Test;

/**
 * Runs tests for {@link MocoServerConfigWriter}.
 */
public class MocoServerConfigWriterTest extends MocoServerTest
  {
  @Test
  public void forOpenApiTest()
    {
    writeMocoServerConfig( "OpenApiTest");
    }

  }
