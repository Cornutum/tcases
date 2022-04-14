//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import org.junit.Test;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.junit.Assert.*;

import static java.util.Collections.emptyMap;

/**
 * Runs tests for {@link MediaRange}.
 */
public class MediaRangeTest
  {
  /**
   * Tests {@link MediaRange#Of Of()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Of (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Subtype </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Suffix </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.Separator </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Name </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Equals </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Value </TD> <TD> Defined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Of_0()
    {
    // Given...
    String definition = "image/svg+xml;height=1024;width=768";
                                
    // When...
    MediaRange mediaRange = MediaRange.of( definition);
    
    // Then...
    assertEquals( "Type", "image", mediaRange.type());
    assertEquals( "Subtype", "svg", mediaRange.subtype());
    assertEquals( "Suffix", "xml", mediaRange.suffix());
    assertEquals( "Parameters", params().put( "height", "1024").put( "width", "768").build(), mediaRange.parameters());
    assertEquals( "String", definition, mediaRange.toString());
    }

  /**
   * Tests {@link MediaRange#Of Of()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Of (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Subtype </TD> <TD> * </TD> </TR>
   * <TR><TD> Suffix </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Count </TD> <TD> None </TD> </TR>
   * <TR><TD> Parameters.Separator </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Name </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Equals </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Value </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Of_1()
    {
    // Given...
    String definition = "application/*+json";
                                
    // When...
    MediaRange mediaRange = MediaRange.of( definition);
    
    // Then...
    assertEquals( "Type", "application", mediaRange.type());
    assertEquals( "Subtype", "*", mediaRange.subtype());
    assertEquals( "Suffix", "json", mediaRange.suffix());
    assertEquals( "Parameters", emptyMap(), mediaRange.parameters());
    assertEquals( "String", definition, mediaRange.toString());
    }

  /**
   * Tests {@link MediaRange#Of Of()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Of (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> * </TD> </TR>
   * <TR><TD> Subtype </TD> <TD> * </TD> </TR>
   * <TR><TD> Suffix </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.Separator </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Name </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Equals </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Value </TD> <TD> Defined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Of_2()
    {
    // Given...
    String definition = "*/*+json;version=1.0.0";
                                
    // When...
    MediaRange mediaRange = MediaRange.of( definition);
    
    // Then...
    assertEquals( "Type", "*", mediaRange.type());
    assertEquals( "Subtype", "*", mediaRange.subtype());
    assertEquals( "Suffix", "json", mediaRange.suffix());
    assertEquals( "Parameters", params().put( "version", "1.0.0").build(), mediaRange.parameters());
    assertEquals( "String", definition, mediaRange.toString());
    }

  /**
   * Tests {@link MediaRange#Of Of()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Of (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> * </TD> </TR>
   * <TR><TD> Subtype </TD> <TD> * </TD> </TR>
   * <TR><TD> Suffix </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.Separator </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Name </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Equals </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Value </TD> <TD> Defined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Of_3()
    {
    // Given...
    String definition = "*/*;level=0;version=\"1.2.3\"";
                                
    // When...
    MediaRange mediaRange = MediaRange.of( definition);
    
    // Then...
    assertEquals( "Type", "*", mediaRange.type());
    assertEquals( "Subtype", "*", mediaRange.subtype());
    assertEquals( "Suffix", null, mediaRange.suffix());
    assertEquals( "Parameters", params().put( "level", "0").put( "version", "1.2.3").build(), mediaRange.parameters());
    assertEquals( "String", definition, mediaRange.toString());
    }

  /**
   * Tests {@link MediaRange#Of Of()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Of (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Subtype </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Suffix </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Parameters.Count </TD> <TD> One </TD> </TR>
   * <TR><TD> Parameters.Separator </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Name </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Equals </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Value </TD> <TD> Defined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Of_4()
    {
    // Given...
    String definition = "text/plain;lang=\"American \\(Standard\\) English\"";
                                
    // When...
    MediaRange mediaRange = MediaRange.of( definition);
    
    // Then...
    assertEquals( "Type", "text", mediaRange.type());
    assertEquals( "Subtype", "plain", mediaRange.subtype());
    assertEquals( "Suffix", null, mediaRange.suffix());
    assertEquals( "Parameters", params().put( "lang", "American (Standard) English").build(), mediaRange.parameters());
    assertEquals( "String", definition, mediaRange.toString());
    }

  /**
   * Tests {@link MediaRange#Of Of()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Of (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> * </TD> </TR>
   * <TR><TD> Subtype </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Suffix </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> Parameters.Count </TD> <TD> None </TD> </TR>
   * <TR><TD> Parameters.Separator </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Name </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Equals </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Parameters.Value </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Of_5()
    {
    // Given...
    String definition = "*/xml";
                                
    // When...
    MediaRange mediaRange = MediaRange.of( definition);
    
    // Then...
    assertEquals( "Type", "*", mediaRange.type());
    assertEquals( "Subtype", "xml", mediaRange.subtype());
    assertEquals( "Suffix", null, mediaRange.suffix());
    assertEquals( "Parameters", emptyMap(), mediaRange.parameters());
    assertEquals( "String", definition, mediaRange.toString());
    }

  /**
   * Tests {@link MediaRange#Of Of()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Of (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Subtype </TD> <TD> <FONT color="red"> Incomplete-Prefix  </FONT> </TD> </TR>
   * <TR><TD> Suffix </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.Separator </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Name </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Equals </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Value </TD> <TD> Defined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Of_6()
    {
    expectFailure( IllegalArgumentException.class)
      .when( () -> MediaRange.of( "application/+json;version=1.0.0"));
    }

  /**
   * Tests {@link MediaRange#Of Of()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Of (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Subtype </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Suffix </TD> <TD> <FONT color="red"> Incomplete  </FONT> </TD> </TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.Separator </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Name </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Equals </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Value </TD> <TD> Defined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Of_7()
    {
    expectFailure( IllegalArgumentException.class)
      .when( () -> MediaRange.of( "application/openapi+"));
    }

  /**
   * Tests {@link MediaRange#Of Of()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Of (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Subtype </TD> <TD> <FONT color="red"> Missing  </FONT> </TD> </TR>
   * <TR><TD> Suffix </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.Separator </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Name </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Equals </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Value </TD> <TD> Defined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Of_8()
    {
    expectFailure( IllegalArgumentException.class)
      .when( () -> MediaRange.of( "application/;version=1.0.0"));
    }

  /**
   * Tests {@link MediaRange#Of Of()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Of (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Subtype </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Suffix </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.Separator </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Name </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Equals </TD> <TD> <FONT color="red"> Missing  </FONT> </TD> </TR>
   * <TR><TD> Parameters.Value </TD> <TD> Defined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Of_9()
    {
    expectFailure( IllegalArgumentException.class)
      .when( () -> MediaRange.of( "application/openapi+json;version=1.0.0;lang EN"));
    }

  /**
   * Tests {@link MediaRange#Of Of()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. Of (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> <FONT color="red"> Missing  </FONT> </TD> </TR>
   * <TR><TD> Subtype </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Suffix </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.Separator </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Name </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Equals </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Value </TD> <TD> Defined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Of_10()
    {
    expectFailure( IllegalArgumentException.class)
      .when( () -> MediaRange.of( "/*+json"));
    }

  /**
   * Tests {@link MediaRange#Of Of()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11. Of (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Subtype </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Suffix </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.Separator </TD> <TD> <FONT color="red"> Missing  </FONT> </TD> </TR>
   * <TR><TD> Parameters.Name </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Equals </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Value </TD> <TD> Defined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Of_11()
    {
    expectFailure( IllegalArgumentException.class)
      .when( () -> MediaRange.of( "application/*+json version=1.0.0"));
    }

  /**
   * Tests {@link MediaRange#Of Of()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12. Of (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Subtype </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Suffix </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.Separator </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Name </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Equals </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Value </TD> <TD> <FONT color="red"> Missing  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Of_12()
    {
    expectFailure( IllegalArgumentException.class)
      .when( () -> MediaRange.of( "application/openpi+json;version="));
    }

  /**
   * Tests {@link MediaRange#Of Of()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 13. Of (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Type </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Subtype </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Suffix </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Count </TD> <TD> Many </TD> </TR>
   * <TR><TD> Parameters.Separator </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Name </TD> <TD> <FONT color="red"> Missing  </FONT> </TD> </TR>
   * <TR><TD> Parameters.Equals </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Parameters.Value </TD> <TD> Defined </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Of_13()
    {
    expectFailure( IllegalArgumentException.class)
      .when( () -> MediaRange.of( "application/json;=1.0.0"));
    }

  private MapBuilder<String,String> params()
    {
    return new MapBuilder<String,String>();
    }

  }
