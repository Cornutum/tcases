package org.cornutum.tcases.util;

import org.junit.Test;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

public class FilenameUtilsTest
  {

  @Test
  public void testGetBaseName()
    {
    assertThat( FilenameUtils.getBaseName( null), nullValue());
    assertThat( FilenameUtils.getBaseName( ""), is(""));
    assertThat( FilenameUtils.getBaseName( "."), is(""));
    assertThat( FilenameUtils.getBaseName( "/"), is(""));
    assertThat( FilenameUtils.getBaseName( "\\"), is(""));

    assertThat( FilenameUtils.getBaseName( "foo"), is("foo"));
    assertThat( FilenameUtils.getBaseName( "foo."), is("foo"));
    assertThat( FilenameUtils.getBaseName( "/foo"), is("foo"));
    assertThat( FilenameUtils.getBaseName( "\\foo"), is("foo"));

    assertThat( FilenameUtils.getBaseName( "bim.bam/foo"), is("foo"));
    assertThat( FilenameUtils.getBaseName( "bim.bam\\foo"), is("foo"));

    assertThat( FilenameUtils.getBaseName( "foo.bar.txt"), is("foo.bar"));
    assertThat( FilenameUtils.getBaseName( ".foo.bar.txt"), is(".foo.bar"));
    assertThat( FilenameUtils.getBaseName( "/foo.bar.txt"), is("foo.bar"));
    assertThat( FilenameUtils.getBaseName( "\\foo.bar.txt"), is("foo.bar"));

    assertThat( FilenameUtils.getBaseName( "bim.bam/foo.bar.txt"), is("foo.bar"));
    assertThat( FilenameUtils.getBaseName( "bim.bam\\foo.bar.txt"), is("foo.bar"));
    }
  }
