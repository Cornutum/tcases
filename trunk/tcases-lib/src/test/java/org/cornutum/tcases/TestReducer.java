//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2014, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.Reducer.Options;

import org.junit.Test;
import static org.junit.Assert.*;

import org.apache.commons.io.FileUtils;

import java.io.File;
import java.net.URL;

/**
 * Runs tests for {@link Reducer#main}.
 *
 * @version $Revision: 262 $, $Date: 2014-04-20 22:21:33 -0500 (Sun, 20 Apr 2014) $
 */
public class TestReducer
  {
  @Test
  public void smoke() throws Exception
    {
    // Given...
    File inFile = new File( "/Users/kerry.kimbrough/repos/magpie-qa/portalroi-test/src/test/tcases/Cir-View-Gaps");
    
    String[] args =
      {
        inFile.getPath()
      };
    
    // When...
    Reducer reducer = new Reducer();
    reducer.run( new Options( args));
        
    // Then...
    }
  }
