using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AdaGrammar
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length == 0)
            {
                Console.WriteLine("Error: No arguments given");
                Console.WriteLine("Usage: AdaGrammar.exe <filename>");
                return;
            }

            StreamReader stream = null;

            try
            {
                stream = new StreamReader(args[0]);
            }
            catch (Exception ex)
            {
                Console.WriteLine("Error: {0}", ex.Message);
                return;
            }

            Stopwatch stopwatch = Stopwatch.StartNew();

            using (stream)
            {
                try
                {
                    var input = new Antlr4.Runtime.AntlrInputStream(stream);
                    AdaLexer lexer = new AdaLexer(input);
                    CommonTokenStream tokens = new CommonTokenStream(lexer);
                    AdaParser parser = new AdaParser(tokens);

                    var result = parser.compilation_unit();

                }
                finally
                {
                    stream.Close();
                }
            }

            stopwatch.Stop();
            Console.WriteLine("Parse Elapsed time: {0}", stopwatch.Elapsed);
        }
    }
}
