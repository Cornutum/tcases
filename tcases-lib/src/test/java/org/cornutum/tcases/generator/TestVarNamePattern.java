//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Runs tests for {@link VarNamePattern}
 *
 */
public class TestVarNamePattern
  {
  /**
   * Tests {@link VarNamePattern#isValid isValid()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0.   isValid (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> String </TD> <TD> NonBlank </TD></TR>
   * <TR><TD> Length </TD> <TD> Many </TD></TR>
   * <TR><TD> Name </TD> <TD> Identifier </TD></TR>
   * <TR><TD> Wildcard.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Wildcard.Position </TD> <TD> Last </TD></TR>
   * <TR><TD> Wildcard.Type </TD> <TD> AllDescendants </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testIsValid_0()
    {
    VarNamePattern pattern = new VarNamePattern ( "1two.THREE4.5-6.seven_Eight.**");
    assertEquals( "pattern=" + pattern + " valid", true, pattern.isValid());
    }

  /**
   * Tests {@link VarNamePattern#isValid isValid()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1.   isValid (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> String </TD> <TD> NonBlank </TD></TR>
   * <TR><TD> Length </TD> <TD> One </TD></TR>
   * <TR><TD> Name </TD> <TD> Identifier </TD></TR>
   * <TR><TD> Wildcard.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Wildcard.Position </TD> <TD> NA </TD></TR>
   * <TR><TD> Wildcard.Type </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testIsValid_1()
    {
    VarNamePattern pattern = new VarNamePattern ( "var");
    assertEquals( "pattern=" + pattern + " valid", true, pattern.isValid());
    }

  /**
   * Tests {@link VarNamePattern#isValid isValid()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2.   isValid (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> String </TD> <TD> NonBlank </TD></TR>
   * <TR><TD> Length </TD> <TD> One </TD></TR>
   * <TR><TD> Name </TD> <TD> Identifier </TD></TR>
   * <TR><TD> Wildcard.Count </TD> <TD> Only </TD></TR>
   * <TR><TD> Wildcard.Position </TD> <TD> Last </TD></TR>
   * <TR><TD> Wildcard.Type </TD> <TD> AllChildren </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testIsValid_2()
    {
    VarNamePattern pattern = new VarNamePattern ( "*");
    assertEquals( "pattern=" + pattern + " valid", true, pattern.isValid());
    }

  /**
   * Tests {@link VarNamePattern#isValid isValid()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3.   isValid (Failure: String) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> String </TD> <TD><FONT color=red> Null </FONT></TD></TR>
   * <TR><TD> Length </TD> <TD> Many </TD></TR>
   * <TR><TD> Name </TD> <TD> Identifier </TD></TR>
   * <TR><TD> Wildcard.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Wildcard.Position </TD> <TD> Last </TD></TR>
   * <TR><TD> Wildcard.Type </TD> <TD> AllDescendants </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testIsValid_3()
    {
    VarNamePattern pattern = new VarNamePattern ( null);
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());
    }

  /**
   * Tests {@link VarNamePattern#isValid isValid()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4.   isValid (Failure: String) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> String </TD> <TD><FONT color=red> Empty </FONT></TD></TR>
   * <TR><TD> Length </TD> <TD> One </TD></TR>
   * <TR><TD> Name </TD> <TD> Identifier </TD></TR>
   * <TR><TD> Wildcard.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Wildcard.Position </TD> <TD> NA </TD></TR>
   * <TR><TD> Wildcard.Type </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testIsValid_4()
    {
    VarNamePattern pattern = new VarNamePattern ( "");
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());
    }

  /**
   * Tests {@link VarNamePattern#isValid isValid()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5.   isValid (Failure: String) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> String </TD> <TD><FONT color=red> Blank </FONT></TD></TR>
   * <TR><TD> Length </TD> <TD> One </TD></TR>
   * <TR><TD> Name </TD> <TD> Identifier </TD></TR>
   * <TR><TD> Wildcard.Count </TD> <TD> Only </TD></TR>
   * <TR><TD> Wildcard.Position </TD> <TD> Last </TD></TR>
   * <TR><TD> Wildcard.Type </TD> <TD> AllChildren </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testIsValid_5()
    {
    VarNamePattern pattern = new VarNamePattern ( "   ");
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());
    }

  /**
   * Tests {@link VarNamePattern#isValid isValid()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6.   isValid (Failure: Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> String </TD> <TD> NonBlank </TD></TR>
   * <TR><TD> Length </TD> <TD> Many </TD></TR>
   * <TR><TD> Name </TD> <TD><FONT color=red> Empty </FONT></TD></TR>
   * <TR><TD> Wildcard.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Wildcard.Position </TD> <TD> Last </TD></TR>
   * <TR><TD> Wildcard.Type </TD> <TD> AllDescendants </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testIsValid_6()
    {
    VarNamePattern pattern;

    pattern = new VarNamePattern ( "first.");
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());

    pattern = new VarNamePattern ( "first..last");
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());

    pattern = new VarNamePattern ( "first.last.");
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());

    pattern = new VarNamePattern ( ".first.last");
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());
    }

  /**
   * Tests {@link VarNamePattern#isValid isValid()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7.   isValid (Failure: Name) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> String </TD> <TD> NonBlank </TD></TR>
   * <TR><TD> Length </TD> <TD> One </TD></TR>
   * <TR><TD> Name </TD> <TD><FONT color=red> NonIdentifier </FONT></TD></TR>
   * <TR><TD> Wildcard.Count </TD> <TD> None </TD></TR>
   * <TR><TD> Wildcard.Position </TD> <TD> NA </TD></TR>
   * <TR><TD> Wildcard.Type </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testIsValid_7()
    {
    VarNamePattern pattern;

    pattern = new VarNamePattern ( " foo.bar");
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());

    pattern = new VarNamePattern ( "foo .bar");
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());

    pattern = new VarNamePattern ( "foo:bar");
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());

    pattern = new VarNamePattern ( "foo.bar ");
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());
    }

  /**
   * Tests {@link VarNamePattern#isValid isValid()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8.   isValid (Failure: Wildcard.Count) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> String </TD> <TD> NonBlank </TD></TR>
   * <TR><TD> Length </TD> <TD> Many </TD></TR>
   * <TR><TD> Name </TD> <TD> Identifier </TD></TR>
   * <TR><TD> Wildcard.Count </TD> <TD><FONT color=red> Many </FONT></TD></TR>
   * <TR><TD> Wildcard.Position </TD> <TD> Last </TD></TR>
   * <TR><TD> Wildcard.Type </TD> <TD> AllDescendants </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testIsValid_8()
    {
    VarNamePattern pattern;

    pattern = new VarNamePattern ( "foo.*.bar.**");
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());

    pattern = new VarNamePattern ( "foo.**.bar.*");
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());
    }

  /**
   * Tests {@link VarNamePattern#isValid isValid()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9.   isValid (Failure: Wildcard.Position) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> String </TD> <TD> NonBlank </TD></TR>
   * <TR><TD> Length </TD> <TD> Many </TD></TR>
   * <TR><TD> Name </TD> <TD> Identifier </TD></TR>
   * <TR><TD> Wildcard.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Wildcard.Position </TD> <TD><FONT color=red> First </FONT></TD></TR>
   * <TR><TD> Wildcard.Type </TD> <TD> AllDescendants </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testIsValid_9()
    {
    VarNamePattern pattern;

    pattern = new VarNamePattern ( "*.foo.bar");
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());

    pattern = new VarNamePattern ( "**.foo");
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());
    }

  /**
   * Tests {@link VarNamePattern#isValid isValid()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10.  isValid (Failure: Wildcard.Position) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> String </TD> <TD> NonBlank </TD></TR>
   * <TR><TD> Length </TD> <TD> Many </TD></TR>
   * <TR><TD> Name </TD> <TD> Identifier </TD></TR>
   * <TR><TD> Wildcard.Count </TD> <TD> One </TD></TR>
   * <TR><TD> Wildcard.Position </TD> <TD><FONT color=red> Middle </FONT></TD></TR>
   * <TR><TD> Wildcard.Type </TD> <TD> AllDescendants </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testIsValid_10()
    {
    VarNamePattern pattern;

    pattern = new VarNamePattern ( "foo.*.bar");
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());

    pattern = new VarNamePattern ( "foo.bar.**.baz");
    assertEquals( "pattern=" + pattern + " valid", false, pattern.isValid());
    }

  /**
   * Tests {@link VarNamePattern#matches matches()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0.   matches (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Pattern.Length </TD> <TD> One </TD></TR>
   * <TR><TD> Pattern.Wildcard </TD> <TD> None </TD></TR>
   * <TR><TD> Path-Head.Length </TD> <TD> Equal </TD></TR>
   * <TR><TD> Path-Head.Match </TD> <TD> Yes </TD></TR>
   * <TR><TD> Path-Tail.Length </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testMatches_0()
    {
    VarNamePattern pattern = new VarNamePattern ( "var");
    String string = "var";

    assertEquals
      ( "pattern=" + pattern + " matches string=" + string,
        true,
        pattern.matches( string));
    }

  /**
   * Tests {@link VarNamePattern#matches matches()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1.   matches (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Pattern.Length </TD> <TD> Many </TD></TR>
   * <TR><TD> Pattern.Wildcard </TD> <TD> AllChildren </TD></TR>
   * <TR><TD> Path-Head.Length </TD> <TD> Equal </TD></TR>
   * <TR><TD> Path-Head.Match </TD> <TD> Yes </TD></TR>
   * <TR><TD> Path-Tail.Length </TD> <TD> One </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testMatches_1()
    {
    VarNamePattern pattern = new VarNamePattern ( "1.2.3.*");
    String string = "1.2.3.4";

    assertEquals
      ( "pattern=" + pattern + " matches string=" + string,
        true,
        pattern.matches( string));
    }

  /**
   * Tests {@link VarNamePattern#matches matches()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2.   matches (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Pattern.Length </TD> <TD> One </TD></TR>
   * <TR><TD> Pattern.Wildcard </TD> <TD> AllDescendants </TD></TR>
   * <TR><TD> Path-Head.Length </TD> <TD> Equal </TD></TR>
   * <TR><TD> Path-Head.Match </TD> <TD> Yes </TD></TR>
   * <TR><TD> Path-Tail.Length </TD> <TD> Many </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testMatches_2()
    {
    VarNamePattern pattern = new VarNamePattern ( "**");
    String string = "1.2.3.4";

    assertEquals
      ( "pattern=" + pattern + " matches string=" + string,
        true,
        pattern.matches( string));
    }

  /**
   * Tests {@link VarNamePattern#matches matches()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3.   matches (Failure: Path-Head.Length) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Pattern.Length </TD> <TD> Many </TD></TR>
   * <TR><TD> Pattern.Wildcard </TD> <TD> AllChildren </TD></TR>
   * <TR><TD> Path-Head.Length </TD> <TD><FONT color=red> Less-Than </FONT></TD></TR>
   * <TR><TD> Path-Head.Match </TD> <TD> Yes </TD></TR>
   * <TR><TD> Path-Tail.Length </TD> <TD> One </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testMatches_3()
    {
    VarNamePattern pattern = new VarNamePattern ( "1.2.3.*");
    String string = "1.2";

    assertEquals
      ( "pattern=" + pattern + " matches string=" + string,
        false,
        pattern.matches( string));
    }

  /**
   * Tests {@link VarNamePattern#matches matches()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4.   matches (Failure: Path-Head.Length) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Pattern.Length </TD> <TD> One </TD></TR>
   * <TR><TD> Pattern.Wildcard </TD> <TD> None </TD></TR>
   * <TR><TD> Path-Head.Length </TD> <TD><FONT color=red> Greater-Than </FONT></TD></TR>
   * <TR><TD> Path-Head.Match </TD> <TD> Yes </TD></TR>
   * <TR><TD> Path-Tail.Length </TD> <TD> NA </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testMatches_4()
    {
    VarNamePattern pattern = new VarNamePattern ( "var");
    String string = "var.extra";

    assertEquals
      ( "pattern=" + pattern + " matches string=" + string,
        false,
        pattern.matches( string));
    }

  /**
   * Tests {@link VarNamePattern#matches matches()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5.   matches (Failure: Path-Head.Match) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Pattern.Length </TD> <TD> Many </TD></TR>
   * <TR><TD> Pattern.Wildcard </TD> <TD> AllChildren </TD></TR>
   * <TR><TD> Path-Head.Length </TD> <TD> Equal </TD></TR>
   * <TR><TD> Path-Head.Match </TD> <TD><FONT color=red> No </FONT></TD></TR>
   * <TR><TD> Path-Tail.Length </TD> <TD> One </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testMatches_5()
    {
    VarNamePattern pattern = new VarNamePattern ( "1.2.3.*");
    String string = "1.2.4.5";

    assertEquals
      ( "pattern=" + pattern + " matches string=" + string,
        false,
        pattern.matches( string));
    }

  /**
   * Tests {@link VarNamePattern#matches matches()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6.   matches (Failure: Path-Tail.Length) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Pattern.Length </TD> <TD> Many </TD></TR>
   * <TR><TD> Pattern.Wildcard </TD> <TD> AllChildren </TD></TR>
   * <TR><TD> Path-Head.Length </TD> <TD> Equal </TD></TR>
   * <TR><TD> Path-Head.Match </TD> <TD> Yes </TD></TR>
   * <TR><TD> Path-Tail.Length </TD> <TD><FONT color=red> None </FONT></TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testMatches_6()
    {
    VarNamePattern pattern = new VarNamePattern ( "1.2.3.*");
    String string = "1.2.3";

    assertEquals
      ( "pattern=" + pattern + " matches string=" + string,
        false,
        pattern.matches( string));
    }

  /**
   * Tests {@link VarNamePattern#matches matches()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7.   matches (Failure: Path-Tail.Length) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Pattern.Length </TD> <TD> Many </TD></TR>
   * <TR><TD> Pattern.Wildcard </TD> <TD> AllChildren </TD></TR>
   * <TR><TD> Path-Head.Length </TD> <TD> Equal </TD></TR>
   * <TR><TD> Path-Head.Match </TD> <TD> Yes </TD></TR>
   * <TR><TD> Path-Tail.Length </TD> <TD><FONT color=red> TooMany </FONT></TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testMatches_7()
    {
    VarNamePattern pattern = new VarNamePattern ( "1.2.3.*");
    String string = "1.2.3.4.5";

    assertEquals
      ( "pattern=" + pattern + " matches string=" + string,
        false,
        pattern.matches( string));
    }

  /**
   * Tests {@link VarNamePattern#matches matches()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8.   matches (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Pattern.Length </TD> <TD> Many </TD></TR>
   * <TR><TD> Pattern.Wildcard </TD> <TD> AllDescendants </TD></TR>
   * <TR><TD> Path-Head.Length </TD> <TD> Equal </TD></TR>
   * <TR><TD> Path-Head.Match </TD> <TD> Yes </TD></TR>
   * <TR><TD> Path-Tail.Length </TD> <TD> Optional </TD></TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testMatches_8()
    {
    VarNamePattern pattern = new VarNamePattern ( "1.2.3.**");
    String string = "1.2.3";

    assertEquals
      ( "pattern=" + pattern + " matches string=" + string,
        true,
        pattern.matches( string));
    }
  }



