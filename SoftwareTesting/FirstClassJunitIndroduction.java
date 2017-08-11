import org.joda.time.DateTime;
import org.joda.time.IllegalFieldValueException;
import org.joda.time.Interval;
import org.joda.time.LocalDate;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.junit.Assert;
import org.junit.Test;

public class FirstClassJunitIndroduction
{
    @Test
    public void beforeAfterDay()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime DateHour2 = new DateTime( 1, 1, 2, 1, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( DateHour2.isAfter( DateHour1 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeAfterHour()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime DateHour2 = new DateTime( 1, 1, 1, 2, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( DateHour2.isAfter( DateHour1 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeAfterMinute()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime DateHour2 = new DateTime( 1, 1, 1, 1, 2 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( DateHour2.isAfter( DateHour1 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeAfterMonth()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime DateHour2 = new DateTime( 1, 2, 1, 1, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( DateHour2.isAfter( DateHour1 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeAfterYear()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime DateHour2 = new DateTime( 2, 1, 1, 1, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( DateHour2.isAfter( DateHour1 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeDay()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime DateHour2 = new DateTime( 1, 1, 2, 1, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( DateHour1.isBefore( DateHour2 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeHour()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime DateHour2 = new DateTime( 1, 1, 1, 2, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( DateHour1.isBefore( DateHour2 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeMinute()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime DateHour2 = new DateTime( 1, 1, 1, 1, 2 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( DateHour1.isBefore( DateHour2 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeMonth()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime DateHour2 = new DateTime( 1, 2, 1, 1, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( DateHour1.isBefore( DateHour2 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeYear()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime DateHour2 = new DateTime( 2, 1, 1, 1, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( DateHour1.isBefore( DateHour2 ) );

        // Fixture Teardown
    }

    // http://www.joda.org/joda-time/apidocs/index.html
    // http://www.joda.org/joda-time/userguide.html#Intervals
    @Test
    public void creationDateHour()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour = new DateTime( 1, 1, 1, 1, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertNotNull( DateHour );

        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDateHourFalhaAll()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour = new DateTime( 0, 0, 0, 0, 0 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDateHourFalhaDayHourMinute()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour = new DateTime( 1, 1, -1, -1, -1 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDateHourFalhaHourMinute()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour = new DateTime( 1, 1, 1, -1, -1 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDateHourFalhaMinute()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour = new DateTime( 1, 1, 1, 1, -1 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDateHourFalhaMonthDayHourMinute()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour = new DateTime( 1, -1, -1, -1, -1 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDateHourFalhaYear()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour = new DateTime( -1, 0, 0, 0, 0 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDateHourFalhaYearMonth()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour = new DateTime( -1, -1, 0, 0, 0 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDateHourFalhaYearMonthDay()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour = new DateTime( -1, -1, -1, 0, 0 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDateHourFalhaYearMonthDayHour()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour = new DateTime( -1, -1, -1, -1, 0 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDateHourFalhaYearMonthDayHourMinute()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour = new DateTime( -1, -1, -1, -1, -1 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test
    public void creationDateHourMinus()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime DateHourEsperado = new DateTime( -1, 11, 30, 0, 0 );

        // Exercise SUT
        DateTime DateHoursum = DateHour1.minusYears( 1 );
        DateHoursum = DateHoursum.minusMonths( 1 );
        DateHoursum = DateHoursum.minusDays( 1 );
        DateHoursum = DateHoursum.minusHours( 1 );
        DateHoursum = DateHoursum.minusMinutes( 1 );

        // Result Verification
        Assert.assertEquals( DateHourEsperado, DateHoursum );

        // Fixture Teardown
    }

    @Test
    public void creationDateHourPlus()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime DateHour1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime DateHourEsperado = new DateTime( 2, 2, 2, 2, 2 );

        // Exercise SUT
        DateTime DateHoursum = DateHour1.plusYears( 1 );
        DateHoursum = DateHoursum.plusMonths( 1 );
        DateHoursum = DateHoursum.plusDays( 1 );
        DateHoursum = DateHoursum.plusHours( 1 );
        DateHoursum = DateHoursum.plusMinutes( 1 );

        // Result Verification
        Assert.assertEquals( DateHourEsperado, DateHoursum );

        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDeDateFalhaDayNegative()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( 2017, 12, -1 );
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDeDateFalhaMonthDayNegative()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( 0, -1, -1 );
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDeDateFalhaMonthNegative()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( 2017, -1, 1 );
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDeDateFalhaYearMontDayNegative()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( -1, -1, -1 );
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDeDateFalhaYearMonthNegative()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( -1, -1, 1 );
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDeDateFalhaYearNegative()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( -20, 1, 0 );
        // Result Verification
        // Fixture Teardown
    }

    @Test
    public void creationDeDateNegative()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( -1, 1, 1 );
        // Result Verification
        // Fixture Teardown
    }

    @Test
    public void creationDeDateNegative2()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( -20, 10, 10 );
        // Result Verification
        // Fixture Teardown
    }

    @Test
    public void creationDeDateRandom1()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( 2017, 12, 25 );
        // Result Verification
        Assert.assertEquals( 2017, natal2017.getYear() );
        Assert.assertEquals( 12, natal2017.getMonthOfYear() );
        Assert.assertEquals( 25, natal2017.getDayOfMonth() );
        // Fixture Teardown
    }

    @Test
    public void creationDeDateRandom2()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( 2017, 1, 25 );
        // Result Verification
        Assert.assertEquals( 2017, natal2017.getYear() );
        Assert.assertEquals( 1, natal2017.getMonthOfYear() );
        Assert.assertEquals( 25, natal2017.getDayOfMonth() );
        // Fixture Teardown
    }

    @Test
    public void creationDeDateYearNegative()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( -1, 1, 1 );
        // Result Verification
        // Fixture Teardown
    }

    @Test
    public void creationDeHours()
    {
        // Fixture Setup
        final LocalTime HourAtual = new LocalTime( 12, 12, 25 );
        final LocalTime HourEsperada = new LocalTime( 11, 12, 25 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( HourAtual.getHourOfDay() > HourEsperada.getHourOfDay() );

        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void creationDeHoursFalha()
    {
        // Fixture Setup
        final LocalTime HourAtual = new LocalTime( -12, 12, 25 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test
    public void creationDeIntervalos()
    {
        // Fixture Setup
        final Interval Hour1 = new Interval( 0, 50 );

        // Exercise SUT

        // Result Verification
        Assert.assertNotNull( Hour1 );

        // Fixture Teardown
    }

    @Test( expected = IllegalArgumentException.class )
    public void creationDeIntervalosFalha()
    {
        // Fixture Setup
        final Interval Hour1 = new Interval( 0, -50 );

        // Exercise SUT

        // Result Verification
        Assert.assertNotNull( Hour1 );

        // Fixture Teardown
    }

    @Test
    public void creationDeIntervalosVazios()
    {
        // Fixture Setup
        final Interval Hour1 = new Interval( 0, 0 );

        // Exercise SUT

        // Result Verification
        Assert.assertNotNull( Hour1 );

        // Fixture Teardown
    }

    @Test
    public void creationDeIntervalosVaziosNegativos()
    {
        // Fixture Setup
        final Interval Hour1 = new Interval( -1, -1 );

        // Exercise SUT

        // Result Verification
        Assert.assertNotNull( Hour1 );

        // Fixture Teardown
    }

    @Test
    public void creationDePeriodos()
    {
        // Fixture Setup
        final Period Hour1 = new Period( 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertNotNull( Hour1 );

        // Fixture Teardown
    }

    @Test( expected = IllegalArgumentException.class )
    public void creationDePeriodosFalha()
    {
        // Fixture Setup
        final Period Hour1 = new Period( "1" );

        // Exercise SUT

        // Result Verification

        // Fixture Teardown
    }

    @Test
    public void criaDateDeAnoBissextoValido()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate dia29Fevereiro = new LocalDate( 2016, 2, 29 );
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void criaDateFalhaDeAnoBissextoInvalido()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate dia29Fevereiro = new LocalDate( 2017, 2, 29 );
        // Result Verification
        // Fixture Teardown
    }

    @Test
    public void criaDateNatal2017()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( 2017, 12, 25 );
        // Result Verification
        Assert.assertEquals( 2017, natal2017.getYear() );
        Assert.assertEquals( 12, natal2017.getMonthOfYear() );
        Assert.assertEquals( 25, natal2017.getDayOfMonth() );
        // Fixture Teardown
    }

    @Test
    public void subtraçãoDeDates()
    {
        // Fixture Setup
        final LocalDate natal2017 = new LocalDate( 2017, 12, 25 );
        final LocalDate natal2016 = new LocalDate( 2016, 12, 25 );

        // Exercise SUT
        final LocalDate ultimoNatal = natal2017.minusYears( 1 );

        // Result Verification
        Assert.assertEquals( natal2016, ultimoNatal );

        // Fixture Teardown
    }

    @Test
    public void subtraçãoDeDatesFalha()
    {
        // Fixture Setup
        final LocalDate natal2017 = new LocalDate( 2017, 12, 25 );
        final LocalDate natal2016 = new LocalDate( 2018, 12, 25 );

        // Exercise SUT
        final LocalDate ultimoNatal = natal2017.minusYears( 2 );

        // Result Verification
        Assert.assertNotEquals( natal2016, ultimoNatal );

        // Fixture Teardown
    }

    @Test
    public void sumDeAnos()
    {
        // Fixture Setup
        final LocalDate natal2017 = new LocalDate( 2017, 12, 25 );
        final LocalDate natal2018 = new LocalDate( 2018, 12, 25 );

        // Exercise SUT
        final LocalDate proximoNatal = natal2017.plusYears( 1 );

        // Result Verification
        Assert.assertEquals( natal2018, proximoNatal );

        // Fixture Teardown
    }

    @Test
    public void sumDeAnosFalha()
    {
        // Fixture Setup
        final LocalDate natal2017 = new LocalDate( 2017, 12, 25 );
        final LocalDate natal2018 = new LocalDate( 2018, 12, 25 );

        // Exercise SUT
        final LocalDate proximoNatal = natal2017.plusYears( 2 );

        // Result Verification
        Assert.assertNotEquals( natal2018, proximoNatal );

        // Fixture Teardown
    }

    @Test
    public void sumDeHours()
    {
        // Fixture Setup
        final LocalTime Hour1 = new LocalTime( 1, 12, 25 );
        final LocalTime Hour2 = new LocalTime( 2, 12, 25 );
        final LocalTime HourEsperada = new LocalTime( 3, 12, 25 );

        // Exercise SUT
        final int novaHour = Hour1.getHourOfDay() + Hour2.getHourOfDay();

        // Result Verification
        Assert.assertEquals( novaHour, HourEsperada.getHourOfDay() );

        // Fixture Teardown
    }

    @Test
    public void sumDeHoursFalha()
    {
        // Fixture Setup
        final LocalTime Hour1 = new LocalTime( 1, 12, 25 );
        final LocalTime Hour2 = new LocalTime( 2, 12, 25 );
        final LocalTime HourEsperada = new LocalTime( 2, 12, 25 );

        // Exercise SUT
        final int novaHour = Hour1.getHourOfDay() + Hour2.getHourOfDay();

        // Result Verification
        Assert.assertNotEquals( novaHour, HourEsperada.getHourOfDay() );

        // Fixture Teardown
    }

    @Test
    public void tamanhoDoPeriodoUmAno()
    {
        // Fixture Setup
        final Period periodoUmAno = new Period( new LocalDate( 2017, 12, 25 ), new LocalDate( 2018, 12, 25 ) );

        // Period(int years, int months, int weeks, int days, int hours, int
        // minutes, int seconds, int millis)
        final Period periodoUmAnoEsperado = new Period( 1, 0, 0, 0, 0, 0, 0, 0 );

        // Exercise SUT

        // Result Verification
        Assert.assertEquals( periodoUmAno, periodoUmAnoEsperado );

        // Fixture Teardown
    }

    @Test
    public void tamanhoDoPeriodoUmAnoFalha()
    {
        // Fixture Setup
        final Period periodoUmAno = new Period( new LocalDate( 2017, 12, 25 ), new LocalDate( 2019, 12, 25 ) );

        // Period(int years, int months, int weeks, int days, int hours, int
        // minutes, int seconds, int millis)
        final Period periodoUmAnoEsperado = new Period( 1, 0, 0, 0, 0, 0, 0, 0 );

        // Exercise SUT

        // Result Verification
        Assert.assertNotEquals( periodoUmAno, periodoUmAnoEsperado );

        // Fixture Teardown
    }

    @Test
    public void tamanhoDoPeriodoUmaSemana()
    {
        // Fixture Setup
        final Period periodoUmAno = new Period( new LocalDate( 2017, 12, 18 ), new LocalDate( 2017, 12, 25 ) );

        // Period(int years, int months, int weeks, int days, int hours, int
        // minutes, int seconds, int millis)
        final Period periodoUmAnoEsperado = new Period( 0, 0, 1, 0, 0, 0, 0, 0 );

        // Exercise SUT

        // Result Verification
        Assert.assertEquals( periodoUmAno, periodoUmAnoEsperado );

        // Fixture Teardown
    }

    @Test
    public void tamanhoDoPeriodoUmaSemanaFalha()
    {
        // Fixture Setup
        final Period periodoUmAno = new Period( new LocalDate( 2017, 12, 17 ), new LocalDate( 2017, 12, 25 ) );

        // Period(int years, int months, int weeks, int days, int hours, int
        // minutes, int seconds, int millis)
        final Period periodoUmAnoEsperado = new Period( 0, 0, 1, 0, 0, 0, 0, 0 );

        // Exercise SUT

        // Result Verification
        Assert.assertNotEquals( periodoUmAno, periodoUmAnoEsperado );

        // Fixture Teardown
    }
}
