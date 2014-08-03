<?xml version="1.0"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="text"/> 

  <xsl:param name="a" select="'X'"/>
  <xsl:param name="b" select="'Y'"/>
  <xsl:param name="c" select="'Z'"/>

  <xsl:template match="TestCases">
    <xsl:variable name="systemId" select="@system"/>

    <xsl:for-each select="Function">
      <xsl:variable name="function" select="@name"/>

      <xsl:for-each select="TestCase">
        <xsl:variable name="id" select="@id"/>

        <xsl:value-of select="$systemId"/> 
        <xsl:text>:</xsl:text>
        <xsl:value-of select="$function"/>
        <xsl:text>:</xsl:text>
        <xsl:value-of select="$id"/> 
        <xsl:text>:</xsl:text>
        <xsl:value-of select="$a"/> 
        <xsl:text>:</xsl:text>
        <xsl:value-of select="$b"/>
        <xsl:text>:</xsl:text>
        <xsl:value-of select="$c"/><xsl:text>
</xsl:text>
      </xsl:for-each>

    </xsl:for-each>

  </xsl:template>

</xsl:stylesheet>
