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
    // http://www.joda.org/joda-time/apidocs/index.html
    // http://www.joda.org/joda-time/userguide.html#Intervals
    @Test
    public void criaçãoDataHora()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora = new DateTime( 1, 1, 1, 1, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertNotNull( dataHora );

        // Fixture Teardown
    }

    @Test
    public void beforeAfterMinute()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime dataHora2 = new DateTime( 1, 1, 1, 1, 2 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( dataHora2.isAfter( dataHora1 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeAfterHour()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime dataHora2 = new DateTime( 1, 1, 1, 2, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( dataHora2.isAfter( dataHora1 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeAfterDay()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime dataHora2 = new DateTime( 1, 1, 2, 1, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( dataHora2.isAfter( dataHora1 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeAfterMonth()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime dataHora2 = new DateTime( 1, 2, 1, 1, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( dataHora2.isAfter( dataHora1 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeAfterYear()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime dataHora2 = new DateTime( 2, 1, 1, 1, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( dataHora2.isAfter( dataHora1 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeBeforeMinute()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime dataHora2 = new DateTime( 1, 1, 1, 1, 2 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( dataHora1.isBefore( dataHora2 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeBeforeHour()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime dataHora2 = new DateTime( 1, 1, 1, 2, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( dataHora1.isBefore( dataHora2 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeBeforeDay()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime dataHora2 = new DateTime( 1, 1, 2, 1, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( dataHora1.isBefore( dataHora2 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeBeforeMonth()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime dataHora2 = new DateTime( 1, 2, 1, 1, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( dataHora1.isBefore( dataHora2 ) );

        // Fixture Teardown
    }

    @Test
    public void beforeBeforeYear()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime dataHora2 = new DateTime( 2, 1, 1, 1, 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( dataHora1.isBefore( dataHora2 ) );

        // Fixture Teardown
    }

    @Test
    public void criaçãoDataHoraMinus()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime dataHoraEsperado = new DateTime( -1, 11, 30, 0, 0 );

        // Exercise SUT
        DateTime dataHoraSoma = dataHora1.minusYears( 1 );
        dataHoraSoma = dataHoraSoma.minusMonths( 1 );
        dataHoraSoma = dataHoraSoma.minusDays( 1 );
        dataHoraSoma = dataHoraSoma.minusHours( 1 );
        dataHoraSoma = dataHoraSoma.minusMinutes( 1 );

        // Result Verification
        Assert.assertEquals( dataHoraEsperado, dataHoraSoma );

        // Fixture Teardown
    }

    @Test
    public void criaçãoDataHoraPlus()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora1 = new DateTime( 1, 1, 1, 1, 1 );
        final DateTime dataHoraEsperado = new DateTime( 2, 2, 2, 2, 2 );

        // Exercise SUT
        DateTime dataHoraSoma = dataHora1.plusYears( 1 );
        dataHoraSoma = dataHoraSoma.plusMonths( 1 );
        dataHoraSoma = dataHoraSoma.plusDays( 1 );
        dataHoraSoma = dataHoraSoma.plusHours( 1 );
        dataHoraSoma = dataHoraSoma.plusMinutes( 1 );

        // Result Verification
        Assert.assertEquals( dataHoraEsperado, dataHoraSoma );

        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void criaçãoDataHoraFalhaMinute()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora = new DateTime( 1, 1, 1, 1, -1 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void criaçãoDataHoraFalhaHourMinute()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora = new DateTime( 1, 1, 1, -1, -1 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void criaçãoDataHoraFalhaDayHourMinute()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora = new DateTime( 1, 1, -1, -1, -1 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void criaçãoDataHoraFalhaMonthDayHourMinute()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora = new DateTime( 1, -1, -1, -1, -1 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void criaçãoDataHoraFalhaYearMonthDayHourMinute()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora = new DateTime( -1, -1, -1, -1, -1 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void criaçãoDataHoraFalhaYearMonthDayHour()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora = new DateTime( -1, -1, -1, -1, 0 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void criaçãoDataHoraFalhaYearMonthDay()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora = new DateTime( -1, -1, -1, 0, 0 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void criaçãoDataHoraFalhaYearMonth()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora = new DateTime( -1, -1, 0, 0, 0 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void criaçãoDataHoraFalhaYear()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora = new DateTime( -1, 0, 0, 0, 0 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void criaçãoDataHoraFalhaAll()
    {
        // Fixture Setup, DateTime(int year, int monthOfYear, int dayOfMonth, int hourOfDay, int minuteOfHour)
        final DateTime dataHora = new DateTime( 0, 0, 0, 0, 0 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test
    public void CriaçãoDeDataRandom1()
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
    public void CriaçãoDeDataRandom2()
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
    public void CriaçãoDeDataNegative()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( -1, 1, 1 );
        // Result Verification
        // Fixture Teardown
    }

    @Test
    public void CriaçãoDeDataNegative2()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( -20, 10, 10 );
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void CriaçãoDeDataFalhaDayNegative()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( 2017, 12, -1 );
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void CriaçãoDeDataFalhaMonthNegative()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( 2017, -1, 1 );
        // Result Verification
        // Fixture Teardown
    }

    @Test
    public void CriaçãoDeDataYearNegative()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( -1, 1, 1 );
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void CriaçãoDeDataFalhaYearMontDayNegative()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( -1, -1, -1 );
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void CriaçãoDeDataFalhaYearMonthNegative()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( -1, -1, 1 );
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void CriaçãoDeDataFalhaMonthDayNegative()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( 0, -1, -1 );
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void CriaçãoDeDataFalhaYearNegative()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate natal2017 = new LocalDate( -20, 1, 0 );
        // Result Verification
        // Fixture Teardown
    }

    @Test
    public void criaçãoDeHoras()
    {
        // Fixture Setup
        final LocalTime horaAtual = new LocalTime( 12, 12, 25 );
        final LocalTime horaEsperada = new LocalTime( 11, 12, 25 );

        // Exercise SUT

        // Result Verification
        Assert.assertTrue( horaAtual.getHourOfDay() > horaEsperada.getHourOfDay() );

        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void criaçãoDeHorasFalha()
    {
        // Fixture Setup
        final LocalTime horaAtual = new LocalTime( -12, 12, 25 );
        // Exercise SUT
        // Result Verification
        // Fixture Teardown
    }

    @Test
    public void criaçãoDeIntervalos()
    {
        // Fixture Setup
        final Interval hora1 = new Interval( 0, 50 );

        // Exercise SUT

        // Result Verification
        Assert.assertNotNull( hora1 );

        // Fixture Teardown
    }

    @Test( expected = IllegalArgumentException.class )
    public void criaçãoDeIntervalosFalha()
    {
        // Fixture Setup
        final Interval hora1 = new Interval( 0, -50 );

        // Exercise SUT

        // Result Verification
        Assert.assertNotNull( hora1 );

        // Fixture Teardown
    }

    @Test
    public void criaçãoDeIntervalosVazios()
    {
        // Fixture Setup
        final Interval hora1 = new Interval( 0, 0 );

        // Exercise SUT

        // Result Verification
        Assert.assertNotNull( hora1 );

        // Fixture Teardown
    }

    @Test
    public void criaçãoDeIntervalosVaziosNegativos()
    {
        // Fixture Setup
        final Interval hora1 = new Interval( -1, -1 );

        // Exercise SUT

        // Result Verification
        Assert.assertNotNull( hora1 );

        // Fixture Teardown
    }

    @Test
    public void criaçãoDePeriodos()
    {
        // Fixture Setup
        final Period hora1 = new Period( 1 );

        // Exercise SUT

        // Result Verification
        Assert.assertNotNull( hora1 );

        // Fixture Teardown
    }

    @Test( expected = IllegalArgumentException.class )
    public void criaçãoDePeriodosFalha()
    {
        // Fixture Setup
        final Period hora1 = new Period( "1" );

        // Exercise SUT

        // Result Verification

        // Fixture Teardown
    }

    @Test
    public void criaDataDeAnoBissextoValido()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate dia29Fevereiro = new LocalDate( 2016, 2, 29 );
        // Result Verification
        // Fixture Teardown
    }

    @Test( expected = IllegalFieldValueException.class )
    public void criaDatAFalhaDeAnoBissextoInvalido()
    {
        // Fixture Setup
        // Exercise SUT
        final LocalDate dia29Fevereiro = new LocalDate( 2017, 2, 29 );
        // Result Verification
        // Fixture Teardown
    }

    @Test
    public void criaDataNatal2017()
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
    public void somaDeAnos()
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
    public void somaDeAnosFalha()
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
    public void somaDeHoras()
    {
        // Fixture Setup
        final LocalTime hora1 = new LocalTime( 1, 12, 25 );
        final LocalTime hora2 = new LocalTime( 2, 12, 25 );
        final LocalTime horaEsperada = new LocalTime( 3, 12, 25 );

        // Exercise SUT
        final int novaHora = hora1.getHourOfDay() + hora2.getHourOfDay();

        // Result Verification
        Assert.assertEquals( novaHora, horaEsperada.getHourOfDay() );

        // Fixture Teardown
    }

    @Test
    public void somaDeHorasFalha()
    {
        // Fixture Setup
        final LocalTime hora1 = new LocalTime( 1, 12, 25 );
        final LocalTime hora2 = new LocalTime( 2, 12, 25 );
        final LocalTime horaEsperada = new LocalTime( 2, 12, 25 );

        // Exercise SUT
        final int novaHora = hora1.getHourOfDay() + hora2.getHourOfDay();

        // Result Verification
        Assert.assertNotEquals( novaHora, horaEsperada.getHourOfDay() );

        // Fixture Teardown
    }

    @Test
    public void subtraçãoDeDatas()
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
    public void subtraçãoDeDatasFalha()
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
