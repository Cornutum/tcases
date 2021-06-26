//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import static java.util.Collections.emptyList;

/**
 * Runs tests for {@link ServerSelector}.
 */
public class ServerSelectorTest
  {
  @Test
  public void whenEmpty()
    {
    // Given...
    List<String> servers = emptyList();
    
    // When...
    Optional<Integer> index = ServerSelector.atIndex(0).select( servers);
    
    // Then...
    assertThat( "Matched", index, is( Optional.empty()));
    
    // Given...
    servers = null;
    
    // When...
    index = ServerSelector.atIndex(0).select( servers);
    
    // Then...
    assertThat( "Matched", index, is( Optional.empty()));
    }
  
  @Test
  public void whenMatchesIndex()
    {
    // Given...
    List<String> servers = Arrays.asList(
      "My Test Server",
      null,
      "My Best Server");
    
    // When...
    Optional<Integer> index = ServerSelector.atIndex(1).select( servers);
    
    // Then...
    assertThat( "Matched", index.isPresent(), is( true));
    assertThat( "Index", index.get(), is( 1));
    
    // When...
    index = ServerSelector.atIndex(3).select( servers);
    
    // Then...
    assertThat( "Matched", index, is( Optional.empty()));
    }
  
  @Test
  public void whenContainsText()
    {
    // Given...
    List<String> servers = Arrays.asList(
      "My Test Server",
      null,
      "My Best Server");
    
    // When...
    Optional<Integer> index = ServerSelector.containing( "Best").select( servers);
    
    // Then...
    assertThat( "Matched", index.isPresent(), is( true));
    assertThat( "Index", index.get(), is( 2));
    
    // When...
    index = ServerSelector.containing( "est").select( servers);
    
    // Then...
    assertThat( "Matched", index.isPresent(), is( true));
    assertThat( "Index", index.get(), is( 0));
    
    // When...
    index = ServerSelector.containing( "?").select( servers);
    
    // Then...
    assertThat( "Matched", index, is( Optional.empty()));
    
    // When...
    index = ServerSelector.containing( null).select( servers);
    
    // Then...
    assertThat( "Matched", index, is( Optional.empty()));
    }

  }
