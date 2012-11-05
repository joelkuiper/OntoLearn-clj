<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:template match="/DescriptorRecordSet/DescriptorRecord">
        <xsl:text>"MSH:</xsl:text><xsl:value-of select="DescriptorUI"></xsl:value-of><xsl:text>","</xsl:text><xsl:value-of select="DescriptorName/String"></xsl:value-of><xsl:text>"</xsl:text>
    </xsl:template>
</xsl:stylesheet>