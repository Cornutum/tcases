//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////
package org.cornutum.tcases.openapi;

import static org.cornutum.tcases.openapi.InputModeller.getServerUrl;

import org.junit.Test;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.servers.Server;

import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

public class ServerUrlTest extends OpenApiTest
  {
  @Test
  public void whenNoVarsUsed()
    {
    // Given...
    OpenAPI api = readApi( "server-url-test");
    Server server = api.getServers().get(0);
    
    // When...
    String serverUrl = getServerUrl( server);
    
    // Then...
    assertThat( "Server URL", serverUrl, is( "https://my-api.org"));
    }
  
  @Test
  public void whenVarsUsed()
    {
    // Given...
    OpenAPI api = readApi( "server-url-test");
    Server server = api.getServers().get(1);
    
    // When...
    String serverUrl = getServerUrl( server);
    
    // Then...
    assertThat( "Server URL", serverUrl, is( "https://my-api.org:123/test-resources"));
    }
  
  @Test
  public void whenVarsUndefined()
    {
    // Given...
    OpenAPI api = readApi( "server-url-test");
    Server server = api.getServers().get(2);

    expectFailure( IllegalStateException.class)
      .when( () -> getServerUrl( server))
      .then( failure -> assertThat( "Failure", failure.getMessage(), is( "No value defined for server variable=port")));
    }
  
  @Test
  public void whenVarUndefined()
    {
    // Given...
    OpenAPI api = readApi( "server-url-test");
    Server server = api.getServers().get(3);

    expectFailure( IllegalStateException.class)
      .when( () -> getServerUrl( server))
      .then( failure -> assertThat( "Failure", failure.getMessage(), is( "No value defined for server variable=version")));
    }
  }
