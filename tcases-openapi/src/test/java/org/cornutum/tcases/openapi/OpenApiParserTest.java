//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.openapi.reader.OpenApiReader;

import io.swagger.v3.oas.models.OpenAPI;
import org.junit.Test;
import static org.apache.commons.lang3.StringUtils.trimToNull;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.File;
import java.util.Optional;

/**
 * Utility for checking results of the OpenAPI parser.
 */
public class OpenApiParserTest extends OpenApiTest
  {
  @Test
  public void parse()
    {
    // Given...
    if( apiFile_ != null)
      {
      // When...
      OpenAPI api;
      try( OpenApiReader reader = new OpenApiReader( apiFile_))
        {
        api = reader.read();
        }

      // Then...
      assertThat( "OpenAPI spec valid", api != null, is( !failureExpected_));
      }
    }

  private final File apiFile_ =
    Optional.ofNullable( trimToNull( System.getProperty( "api")))
    .map( path -> new File( path))
    .orElse( null);

  private final boolean failureExpected_ =
    Optional.ofNullable( trimToNull( System.getProperty( "failure")))
    .map( failure -> Boolean.parseBoolean( failure))
    .orElse( false);
  }
